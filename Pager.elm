module Pager exposing (Model,Msg,init,update,view,doNextItem,doPrevItem)
import IndexedItem exposing (..)
import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick, onSubmit)

type alias Model item msg =
    {
        current: Maybe ItemId,
        items: IndexedList item,
        updateItem: msg -> item -> item,
        viewItem: item -> Html msg
    }

type Msg msg
    = NextItem 
    | PrevItem
    | UpdateItem ItemId msg

currentItem model = case model.current of
    Nothing -> Nothing
    Just n -> nth n model.items

init: (msg -> item -> item) -> (item -> Html msg) -> List item -> Model item msg
init updateItem viewItem items =
    let 
        indexedList = List.indexedMap indexItem items
        currentIndex = case List.head indexedList of
            Nothing -> Nothing
            Just item -> Just item.id
    in
       {current = currentIndex, items = indexedList, updateItem = updateItem, viewItem = viewItem}

nth : Int -> List x -> Maybe x
nth n list = List.head (List.drop n list)

update : Msg msg -> Model item msg -> Model item msg
update action model = case action of
    PrevItem ->
        case model.current of 
            Nothing -> model
            Just id -> 
                let
                    next = nth (id-1) model.items
                in
                    case next of
                        Nothing -> model
                        Just i -> {model | current = Just i.id}
    NextItem ->
        case model.current of 
            Nothing -> model
            Just id -> 
                let
                    next = nth (id+1) model.items
                in
                    case next of
                        Nothing -> model
                        Just i -> {model | current = Just i.id}

    UpdateItem id msg ->
        let
           updatedItems = listUpdateItem model.updateItem id msg model.items
        in
           {model | items = updatedItems}

refreshCurrent current items = case current of
    Nothing -> Nothing
    Just x -> nth x.id items

doNextItem = NextItem
doPrevItem = PrevItem

viewItem : Model item msg -> IndexedItem item -> List (Html (Msg msg))
viewItem model item = case (currentItem model) of
    Nothing -> []
    Just currentItem -> 
        let
            visible = item==currentItem
        in
           [
               div [class ("pager-item "++(if visible then "current" else ""))] [Html.map (UpdateItem item.id) (model.viewItem item.item)]
           ]

view : Model item msg -> Html (Msg msg)
view model = div [class "pager"] (List.concatMap (viewItem model) model.items)
