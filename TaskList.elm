module TaskList exposing (ItemProcessor,processQueue,update,Model,ready,Msg)

type alias ItemProcessor a b = (b -> Msg b) -> a -> Cmd (Msg b)
type alias QueueProcessor a b = (ItemProcessor a b) -> (List a) -> Cmd (Msg b)

processQueue : QueueProcessor a b
processQueue processItem queue = Cmd.batch (List.indexedMap (\pos -> processItem (AddItem pos)) queue)

type Msg a
  = FailItem Never
  | AddItem Int a

update : Msg b -> Model a b -> (Model a b, Cmd (Msg a))
update msg model =
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
