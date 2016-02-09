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
    { field : String
    , items : List Item
    , matchedItems : List Item
    , uid : Int
    , index : Maybe Int
    }


type alias Item =
    { desc : String
    , id : Int
    , index : Int
    }


emptyModel : Model
emptyModel =
    { field = ""
    , items = []
    , matchedItems = []
    , uid = 0
    , index = Nothing
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
    | UpdateField String
    | Enter
    | Down
    | Up

update : Action -> Model -> Model
update action model =
    case action of
        NoOp -> model

        UpdateField str ->
            let matchedItems = List.filter contains model.items
                contains item = Regex.contains regex item.desc
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
                { model |
                    field = str,
                    matchedItems = matchedItems
                }

        Enter ->
            let isMatch = not (List.isEmpty model.matchedItems)
                isEmpty = String.isEmpty model.field
                isItems = not (List.isEmpty model.items)
            in
                { model |
                    uid = if isEmpty || isMatch then model.uid else model.uid + 1,
                    field = if isMatch then model.field else "",
                    index = if not isEmpty || isMatch then Just 0 else Nothing,
                    items =
                        if isEmpty || isMatch
                        then model.items
                        else model.items ++
                            [newItem model.field model.uid model.uid]
                }

        Down ->
            let isItems = not (List.isEmpty model.items)
                isMatch = not (List.isEmpty model.matchedItems)
                itemLength = List.length model.items
                update m = Maybe.map2 (+) (Just 1) m
                min m1 m2 = Maybe.map2 Basics.min m1 m2
            in
                {model |
                    index =
                        if isItems
                        then min (Just (itemLength - 1)) (update model.index)
                        else Nothing
                }

        Up ->
            let isItems = not (List.isEmpty model.items)
                isMatch = not (List.isEmpty model.matchedItems)
                update m = (Maybe.map2 (+) (Just -1) m)
                max m1 m2 = Maybe.map2 Basics.max m1 m2
            in
                {model |
                    index =
                        if isItems
                        then max (Just 0) (update model.index)
                        else Nothing
                }


-- VIEW

view : Address Action -> Model -> Html
view address model =
    let isMatch = not (List.isEmpty model.matchedItems)
        isEmpty = String.isEmpty model.field
        items =
            if isMatch then model.matchedItems
            else if isEmpty then model.items
            else []
    in
        div
            []
            [ input
                [ autofocus True
                , value model.field
                , onKeyDown address keyHandler
                , on "input" targetValue (Signal.message address << UpdateField)
                , class "input"
                ]
                []
            , ul
                [ class "list" ]
                (List.map (item address model) items)
            ]

item : Address Action -> Model -> Item -> Html
item address model item  =
    let fontWeight = if Just item.index == model.index then "bold" else "normal"
        borderLeft = if Just item.index == model.index then ".6472rem solid #333" else ""
        paddingLeft = if Just item.index == model.index then "1.294rem" else ""
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

