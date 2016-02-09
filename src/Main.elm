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


-- MODEL

type alias Model =
    { string : String
    , items : List Item
    , uid : Int
    , index : Int
    }


type alias Item =
    { desc : String
    , id : Int
    , index : Int
    }


emptyModel : Model
emptyModel =
    { string = ""
    , items = []
    , uid = 0
    , index = 0
    }


newItem : String -> Int -> Int -> Item
newItem desc id index =
    { desc = desc
    , id = id
    , index = index
    }


-- UPDATE

type Action
    = NoOp
    | UpdateString String
    | Enter
    | Up
    | Down


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
            { model | string = str }

        Enter ->
            { model |
                uid = if addNot model then model.uid else model.uid + 1,
                string = if isMatch model then model.string else "",
                items =
                    if addNot model then model.items
                    else model.items ++ [newItem model.string model.uid model.uid]
                }
        Up ->
            {model |
                index = Basics.max (model.index - 1) 0}
        Down ->
            {model |
                index = Basics.min (model.index + 1) ((List.length model.items) - 1)}



-- VIEW

view : Address Action -> Model -> Html
view address model =
    let items =
            if isMatch model then
                matches model.string model.items
            else model.items
    in
        div
            []
            [ input
                [ autofocus True
                , value model.string
                , onKeyDown address keyHandler
                , on "input" targetValue (Signal.message address << UpdateString)
                , class "input"
                ]
                []
            , ul
                [ class "list" ]
                (List.map (item address model) items)
            ]

item : Address Action -> Model -> Item -> Html
item address model item  =
    let fontWeight = if item.index == model.index then "bold" else "normal"
        borderLeft = if item.index == model.index then ".6472rem solid #333" else ""
        paddingLeft = if item.index == model.index then "1.294rem" else ""
    in
        li
            [ style [ ("font-weight", fontWeight)
                    , ("border-left", borderLeft)
                    , ("padding-left", paddingLeft)
            ]]
            [text item.desc]


keyHandler : Int -> Action
keyHandler code =
    case code of
        13 -> Enter
        40 -> Down
        38 -> Up
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
  Maybe.withDefault emptyModel getStorage


-- actions from user input
actions : Mailbox Action
actions =
  Signal.mailbox NoOp


-- outgoing
port modelLogger : Signal Model
port modelLogger =
    Signal.map (Debug.log "") model


-- interactions with localStorage to save the model
port getStorage : Maybe Model

port setStorage : Signal Model
port setStorage = model

