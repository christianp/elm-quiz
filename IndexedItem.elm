module IndexedItem exposing (IndexedItem,ItemId,IndexedList,indexItem,updateItem,listUpdateItem)

type alias ItemId = Int
type alias IndexedItem model = {id: ItemId, item: model}

type alias IndexedList model = List (IndexedItem model)

indexItem id item = {id = id, item = item}

updateItem : (msg -> model -> model) -> ItemId -> msg -> IndexedItem model -> IndexedItem model
updateItem update targetId msg {id,item} = IndexedItem id (if id==targetId then update msg item else item)

listUpdateItem : (msg -> model -> model) -> ItemId -> msg -> IndexedList model -> IndexedList model
listUpdateItem update targetId msg items = List.map (updateItem update targetId msg) items
