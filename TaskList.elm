module TaskList exposing (ItemProcessor,processQueue,update,Model,ready,Msg)

type alias ItemProcessor a b = (b -> Msg b) -> a -> Cmd (Msg b)
type alias Updater a b msg = Msg b -> Model a b -> (Model a b, Cmd msg)
type alias QueueProcessor a b msg = 
    (Msg b -> msg) 
    -> (ItemProcessor a b) 
    -> (List a) 
    -> (
        Model a b, 
        Cmd msg,
        Updater a b msg
    )

processQueue : QueueProcessor a b msg
processQueue msg processItem queue = 
    (
        {queue = queue, items = List.map (\_ -> Nothing) queue, done = False},
        Cmd.map msg <| Cmd.batch (List.indexedMap (\pos -> processItem (AddItem pos)) queue),
        update msg
    )

type Msg a
  = FailItem Never
  | AddItem Int a

update: (Msg b -> msg) -> Updater a b msg
update msgConstructor myMsg model = 
    let
        (model,cmd) = updateList myMsg model
    in
        (model,Cmd.map msgConstructor cmd)

updateList : Msg b -> Model a b -> (Model a b, Cmd (Msg a))
updateList msg model =
  case msg of
    FailItem failed -> (model, Cmd.none)
    AddItem pos item -> (
      doFinish <| {model | items = List.indexedMap (\i -> \v -> if i==pos then Just item else v) model.items},
      Cmd.none
      )

doFinish : Model a b -> Model a b
doFinish model = {model | done = ready model}

type alias Model a b =
  { 
    queue: List a,
    items: List (Maybe b),
    done: Bool
  }

ready : Model a b -> Bool
ready model = List.all ((/=) Nothing) model.items
