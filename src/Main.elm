module Main where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Json.Decode as Json
import Signal exposing (Signal, Address, Mailbox)
import String
import Window
import Http
import Regex
import Time
import ItemList

-- MODEL

type alias Model =
    { string : String
    , items : List Item
    , uid : Int
    , index : Int
    , showActions : Bool
    }


type alias Item =
    { desc : String
    , id : Int
    , index : Int
    , itemActions : ItemList.Model
    }


init : Model
init =
    { string = ""
    , items = []
    , uid = 0
    , index = 0
    , showActions = False
    }


newItem : String -> Int -> Int -> Item
newItem desc id index =
    { desc = desc
    , id = id
    , index = index
    , itemActions = ItemList.init
    }


-- UPDATE

type Action
    = NoOp
    | UpdateString String
    | Enter
    | Up
    | Down
    | Esc
    | ItemList Int ItemList.Action


matches : String -> List Item -> List Item
matches str items =
    let contains item = Regex.contains regex item.desc
        regex = str
                |> String.split " "
                |> String.join ""
                |> String.toList
                |> List.map
                    (\c -> ".*" ++ (Regex.escape (String.fromChar c)))
                |> String.join ""
                |> Regex.regex
                |> Regex.caseInsensitive
    in
        List.filter contains items


isMatch : Model -> Bool
isMatch model = not (List.isEmpty (matches model.string model.items))


strEmpty : Model -> Bool
strEmpty model = String.isEmpty model.string


addNot : Model -> Bool
addNot model = strEmpty model || isMatch model


update : Action -> Model -> Model
update action model =
    case action of
        NoOp -> model

        UpdateString str ->
            { model |
                string = str,
                index = if isMatch model then 0 else model.index
            }

        Enter ->
            { model |
                uid = if addNot model then model.uid else model.uid + 1,
                string = if isMatch model then model.string else "",
                items =
                    if addNot model then model.items
                    else model.items ++ [newItem model.string model.uid model.uid],
                showActions =
                    if addNot model then True
                    else False
            }

        Up ->
            { model |
                index = Basics.max (model.index - 1) 0
            }

        Down ->
            let matchesLength = List.length (matches model.string model.items)
                itemLength = List.length model.items
            in
                { model |
                    index =
                        if isMatch model
                        then Basics.min (model.index + 1) (matchesLength - 1)
                        else Basics.min (model.index + 1) (itemLength - 1)
                }

        Esc ->
            { model |
                showActions = False,
                string = if model.showActions then model.string else ""}

        ItemList id act ->
            let update item =
                    if item.index == id then
                        { item | itemActions = ItemList.update act item.itemActions }
                    else item
            in
                { model |
                    items = List.map update model.items,
                    showActions =
                        if (toString act) == "Esc" ||
                            (toString act) == "Enter" then False else True
                }



-- VIEW

view : Address Action -> Model -> Html
view address model =
    let items =
            if isMatch model then
                List.indexedMap update (matches model.string model.items)
            else []
        update i item = { item | index = i }
    in
        div
            []
            [ input
                [ autofocus True
                , value model.string
                , onKeyDown address keyHandler
                , on "input" targetValue (Signal.message address << UpdateString)
                , class "input-main"
                ]
                []
            , ul
                [ class "list" ]
                (List.map (item address model) items)
            ]

item : Address Action -> Model -> Item -> Html
item address model item  =
    let selected    = item.index == model.index
        fontWeight  = if selected then "bold" else "normal"
        borderLeft  = if selected then ".6472rem solid #333" else ""
        paddingLeft = if selected then "1.294rem" else ""
        displayActions = if selected && model.showActions then "block" else "none"
        itemActions =
            ItemList.view (Signal.forwardTo address (ItemList item.id)) item.itemActions
    in
        li
            [ style [ ("font-weight", fontWeight)
                    , ("border-left", borderLeft)
                    , ("padding-left", paddingLeft)
                    ]
            ]
            [ text item.desc
            , div [ style [ ("display", displayActions) ] ] [ itemActions ]
            ]


keyHandler : Int -> Action
keyHandler code =
    case code of
        13 -> Enter
        40 -> Down
        38 -> Up
        27 -> Esc
        _ -> NoOp


-- INPUTS

-- manage our application over time
main : Signal Html
main =
  Signal.map (view actions.address) model


model : Signal Model
model =
  Signal.foldp update initialModel actions.signal

initialModel : Model
initialModel =
  Maybe.withDefault init getStorage


-- actions from user input
actions : Mailbox Action
actions =
  Signal.mailbox NoOp


-- outgoing
--port modelLogger : Signal Model
--port modelLogger =
    --Signal.map (Debug.log "") model


port focus : Signal String
port focus =
    let needsFocus act =
            case act of
              Enter -> True
              Esc -> True
              ItemList _ act ->
                  if (toString act) == "Esc" ||
                      (toString act) == "Enter" then True else False
              _ -> False

        toActionString act =
            case act of
                Enter -> "Enter"
                Esc -> "Esc"
                ItemList _ act -> "ItemList " ++ (toString act)
                _ -> ""

    in
        actions.signal
          |> Signal.filter needsFocus Esc
          |> Signal.map toActionString


-- interactions with localStorage to save the model
port getStorage : Maybe Model

port setStorage : Signal Model
port setStorage = model

