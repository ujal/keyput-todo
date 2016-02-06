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
    , itemsIndex : Maybe Int
    , matchedItems : List Item
    , matchedItemsIndex : Maybe Int
    , uid : Int
    }


type alias Item =
    { desc : String
    , id : Int
    }


emptyModel : Model
emptyModel =
    { field = ""
    , items = []
    , itemsIndex = Just 0
    , matchedItems = []
    , matchedItemsIndex = Nothing
    , uid = 0
    }


newItem : String -> Int -> Item
newItem desc id =
    { desc = desc
    , id = id
    }


-- UPDATE

type Action
    = NoOp
    | UpdateField String
    | Add

update : Action -> Model -> Model
update action model =
    case action of
        NoOp -> model

        UpdateField str ->
            let matchedItems  = List.filter contains model.items
                contains item = Regex.contains regex item.desc
                regex = str
                        |> String.split " "
                        |> String.join ""
                        |> String.toList
                        |> List.map (\c -> ".*" ++ (Regex.escape (String.fromChar c)))
                        |> String.join ""
                        |> Regex.regex
                        |> Regex.caseInsensitive
            in
                { model |
                    field = str,
                    matchedItems = matchedItems
                }

        Add ->
            let isMatch = not (List.isEmpty model.matchedItems)
                isEmpty = String.isEmpty model.field
            in
                { model |
                    uid   = if isEmpty || isMatch then model.uid else model.uid + 1,
                    field = if isMatch then model.field else "",
                    items =
                        if isEmpty || isMatch
                           then model.items
                           else model.items ++ [newItem model.field model.uid]
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
                , onEnter address Add
                , on "input" targetValue (Signal.message address << UpdateField)
                ]
                []
            , ul
                []
                (List.map (item address) items)
            ]

item : Address Action -> Item -> Html
item address item =
    li [] [text item.desc]


onEnter : Address a -> a -> Attribute
onEnter address value =
    on "keydown"
      (Json.customDecoder keyCode is13)
      (\_ -> Signal.message address value)


is13 : Int -> Result String ()
is13 code =
  if code == 13 then Ok () else Err "not the right key code"


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
--port modelLogger : Signal Model
--port modelLogger =
    --Signal.map (Debug.log "") model


-- interactions with localStorage to save the model
port getStorage : Maybe Model

port setStorage : Signal Model
port setStorage = model

