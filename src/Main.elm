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
    , showEdit : Bool
    , showNote : Bool
    }


type alias Item =
    { desc : String
    , note : String
    , id : Int
    , index : Int
    , itemActions : ItemList.Model
    , done : Bool
    }


init : Model
init =
    { string = ""
    , items =
        [ newItem "a small but mighty todo app" 0 0
        , newItem "type or search and press enter" 1 1
        , newItem "to add, search or modify your todos" 2 2
        , newItem "you got esc too" 3 3
        ]
    , uid = 4
    , index = 0
    , showActions = False
    , showEdit = False
    , showNote = False
    }


newItem : String -> Int -> Int -> Item
newItem desc id index =
    { desc = desc
    , note = ""
    , id = id
    , index = index
    , itemActions = ItemList.init
    , done = False
    }


-- UPDATE

type Action
    = NoOp
    | UpdateString String
    | EditItem Int String
    | EditEnter
    | EditEsc
    | EditNote Int String
    | EditNoteEnter
    | EditNoteEsc
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
                    index = if isMatch model then 0 else model.index,
                    showActions = False
                }

        EditItem id str ->
            let update item = if item.id == id then { item | desc = str } else item
            in
                { model | items = List.map update model.items }

        EditEnter -> { model | showEdit = False }

        EditEsc -> { model | showEdit = False }

        EditNote id str ->
            let update item = if item.id == id then { item | note = str } else item
            in
                { model | items = List.map update model.items }

        EditNoteEnter -> { model | showNote = False }

        EditNoteEsc -> { model | showNote = False }

        Enter ->
            { model |
                uid = if addNot model then model.uid else model.uid + 1,
                string = if isMatch model then model.string else "",
                items =
                    if addNot model then model.items
                    else model.items ++
                        [newItem model.string model.uid (List.length model.items)],
                showActions =
                    if addNot model then True
                    else False
            }

        Up ->
            { model |
                index = Basics.max (model.index - 1) 0,
                showActions = False
            }

        Down ->
            let matchesLength = List.length (matches model.string model.items)
                itemLength = List.length model.items
            in
                { model |
                    index =
                        if isMatch model
                        then Basics.min (model.index + 1) (matchesLength - 1)
                        else Basics.min (model.index + 1) (itemLength - 1),
                    showActions = False,
                    showNote = False
                }

        Esc ->
            { model |
                showNote = False,
                showActions = False,
                string = if model.showActions then model.string else ""
            }

        ItemList id act ->
            let update item =
                    if item.id == id then
                        { item | itemActions = ItemList.update act item.itemActions }
                    else item
                updateItems items = List.map update items
                toggle i = if i.id == id then { i | done = not i.done } else i
                checkall item = { item | done = True }
            in
                { model |
                    index =
                        case (toString act) of
                            "Esc" -> model.index
                            "Enter Check" -> model.index
                            "Enter Remove" -> 0
                            "Enter Clear" -> 0
                            "Enter CheckAll" -> 0
                            _ -> model.index,
                    string =
                        case (toString act) of
                            "Esc" -> model.string
                            "Enter Check" -> ""
                            "Enter Remove" -> ""
                            "Enter Clear" -> ""
                            "Enter CheckAll" -> ""
                            _ -> model.string,
                    items =
                        case (toString act) of
                            "Esc" -> updateItems model.items
                            "Enter Check" ->
                                List.map toggle (updateItems model.items)
                            "Enter CheckAll" ->
                                List.map checkall (updateItems model.items)
                            "Enter Remove" ->
                                model.items
                                    |> updateItems
                                    |> List.filter (\i -> i.id /= id)
                                    |> List.indexedMap updateIndex
                            "Enter Clear" ->
                                model.items
                                    |> updateItems
                                    |> List.filter (not << .done)
                                    |> List.indexedMap updateIndex
                            _ -> updateItems model.items,
                    showActions =
                        case (toString act) of
                            "Esc" -> False
                            "Enter Check" -> False
                            "Enter Remove" -> False
                            "Enter Clear" -> False
                            "Enter Edit" -> False
                            "Enter CheckAll" -> False
                            "Enter Note" -> False
                            _ -> True,
                    showEdit =
                        case (toString act) of
                            "Esc" -> False
                            "Enter Check" -> False
                            "Enter Remove" -> False
                            "Enter Clear" -> False
                            "Enter Edit" -> True
                            "Enter Note" -> False
                            _ -> False,
                    showNote =
                        case (toString act) of
                            "Esc" -> False
                            "Enter Check" -> False
                            "Enter Remove" -> False
                            "Enter Clear" -> False
                            "Enter Edit" -> False
                            "Enter Note" -> True
                            _ -> False
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
item address model item =
    let selected    = item.index == model.index
        fontWeight  = if selected then "bold" else "normal"
        borderLeft  = if selected then ".6472rem solid #333" else ""
        paddingLeft = if selected then "1.294rem" else ""
        displayActions = if selected && model.showActions then "block" else "none"
        displayEdit = if selected && model.showEdit then "block" else "none"
        displayItem = if selected && model.showEdit then "none" else "block"
        displayNote = if selected && model.showNote then "block" else "none"
        decor = if item.done then "line-through" else "none"
        itemDesc = if item.desc == "" then "#todo" else item.desc
        itemActions =
            ItemList.view
                (Signal.forwardTo address (ItemList item.id))
                item.itemActions
    in
        li
            [ style [ ("font-weight", fontWeight)
                    , ("border-left", borderLeft)
                    , ("padding-left", paddingLeft)
                    ]
            ]
            [ div
                [ style [ ("text-decoration", decor)
                        , ("display", displayItem)
                        ]]
                [ text itemDesc ]
            , div
                [ style [ ("display", displayActions) ] ]
                [ itemActions ]
            , div
                [ style [ ("display", displayEdit)
                        , ("font-weight", "normal")
                        ] ]
                [ input
                    [ class "input-edit"
                    , value item.desc
                    , onKeyDown address editHandler
                    , on
                        "input"
                        targetValue
                        (Signal.message address << EditItem item.id)
                    ]
                    []
                ]
            , div
                [ style [ ("display", displayNote)
                        , ("font-weight", "normal")
                        ] ]
                [ textarea
                    [ class "input-note"
                    , value item.note
                    , rows 3
                    , onKeyDown address noteHandler
                    , on
                        "input"
                        targetValue
                        (Signal.message address << EditNote item.id)
                    ]
                    []
                ]
            ]


keyHandler : Int -> Action
keyHandler code =
    case code of
        13 -> Enter
        40 -> Down
        38 -> Up
        27 -> Esc
        _ -> NoOp


editHandler : Int -> Action
editHandler code =
    case code of
        13 -> EditEnter
        27 -> EditEsc
        _ -> NoOp


noteHandler : Int -> Action
noteHandler code =
    case code of
        13 -> EditNoteEnter
        27 -> EditNoteEsc
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
                EditEnter -> True
                EditEsc -> True
                EditNoteEnter -> True
                EditNoteEsc -> True
                ItemList _ act ->
                    case (toString act) of
                        "Esc" -> True
                        "Enter Check" -> True
                        "Enter CheckAll" -> True
                        "Enter Remove" -> True
                        "Enter Clear" -> True
                        "Enter Edit" -> True
                        "Enter Note" -> True
                        _ -> False
                _ -> False

        toActionString act =
            case act of
                Enter -> "Enter"
                EditEnter -> "ItemList Enter"
                EditEsc -> "ItemList Enter"
                EditNoteEnter -> "ItemList Enter"
                EditNoteEsc -> "ItemList Enter"
                ItemList _ act ->
                    case (toString act) of
                        "Esc" -> "ItemList Esc"
                        "Enter Check" -> "ItemList Enter"
                        "Enter CheckAll" -> "ItemList Enter"
                        "Enter Remove" -> "ItemList Enter"
                        "Enter Clear" -> "ItemList Enter"
                        "Enter Edit" -> "ItemList Edit"
                        "Enter Note" -> "ItemList Note"
                        _ -> ""
                _ -> ""

    in
        actions.signal
          |> Signal.filter needsFocus Esc
          |> Signal.map toActionString


-- interactions with localStorage to save the model
port getStorage : Maybe Model

port setStorage : Signal Model
port setStorage = model

