module ItemList (Model, init, Action, update, view, Context) where

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


-- MODEL

type alias Model =
    { string : String
    , items : List Item
    , index : Int
    }


type alias Item =
    { desc : String
    , index : Int
    }


init : Model
init =
    { string = ""
    , items = items
    , index = 0
    }


items : List Item
items =
    [ newItem "Check" 0
    , newItem "Note" 1
    , newItem "Edit" 2
    , newItem "Remove" 3
    , newItem "––" 4
    , newItem "Clear All" 5
    , newItem "Check All" 6
    ]


newItem : String -> Int -> Item
newItem desc index =
    { desc = desc
    , index = index
    }


-- UPDATE

type alias Context =
    { actions : Signal.Address Action
    , itemEnter : Signal.Address ()
    }

type Action
    = NoOp
    | UpdateString String
    | Enter EnterAction
    | Up
    | Down
    | Esc


type EnterAction = Check | Remove | Clear | Edit | CheckAll | Note


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
        if String.isEmpty str then [] else List.filter contains items


isMatch : Model -> Bool
isMatch model = not (List.isEmpty (matches model.string model.items))


strEmpty : Model -> Bool
strEmpty model = String.isEmpty model.string


addNot : Model -> Bool
addNot model = strEmpty model || isMatch model


updateIndex : Int -> Item -> Item
updateIndex i item = { item | index = i }


update : Action -> Model -> Model
update action model =
    case action of
        NoOp -> model

        UpdateString str ->
            let update i item = { item | index = i }
            in
                { model |
                    string = str,
                    index = if isMatch model then 0 else model.index
                }

        Enter _ ->
            let update i item = { item | index = i }
            in
                { model |
                    string = "",
                    index = 0,
                    items = List.indexedMap update model.items
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
                        if isMatch model then
                            Basics.min (model.index + 1) (matchesLength - 1)
                        else
                            Basics.min (model.index + 1) (itemLength - 1)
                }

        Esc ->
            { model |
                index = 0,
                string = ""
            }



-- VIEW

view : Address Action -> Model -> Html
view address model =
    let items =
            if isMatch model then
                model.items
                    |> matches model.string
                    |> List.indexedMap updateIndex
            else if strEmpty model then model.items
            else []
        update i item = { item | index = i }
    in
        div
            [ class "actions" ]
            [ input
                [ autofocus True
                , value model.string
                , onKeyDown address (keyHandler model)
                , on "input" targetValue (Signal.message address << UpdateString)
                , class "input-actions"
                ]
                []
            , ul
                [ class "list" ]
                (List.map (item address model) items)
            ]

item : Address Action -> Model -> Item -> Html
item address model item =
    let fontWeight = if selected then "bold" else "normal"
        borderLeft = if selected then ".6472rem solid #333" else ""
        paddingLeft = if selected then "1.294rem" else ""
        selected = item.index == model.index
    in
        li
            [ style [ ("font-weight", fontWeight)
                    , ("border-left", borderLeft)
                    , ("padding-left", paddingLeft)
                    ]
            ]
            [text item.desc]



select : Int -> List Item -> Item
select index items =
    let filterf item = item.index == index
    in
        Maybe.withDefault
            (newItem "NoOp" 999)
            (List.head (List.filter filterf items))


keyHandler : Model -> Int -> Action
keyHandler model code =
    case code of
        13 ->
            let selected =
                    if isMatch model then
                        model.items
                            |> matches model.string
                            |> List.indexedMap updateIndex
                            |> select model.index
                    else if not (strEmpty model) then newItem "NoOp" 999
                    else select model.index model.items
                updateIndex i item = { item | index = i }
                _ = Debug.log "" selected
            in
                if selected.desc == "Check" then Enter Check
                else if selected.desc == "––" then Enter Check
                else if selected.desc == "Note" then Enter Note
                else if selected.desc == "Edit" then Enter Edit
                else if selected.desc == "Remove" then Enter Remove
                else if selected.desc == "Clear All" then Enter Clear
                else if selected.desc == "Check All" then Enter CheckAll
                else if selected.desc == "NoOp" then NoOp
                else NoOp
        40 -> Down
        38 -> Up
        27 -> Esc
        _ -> NoOp
