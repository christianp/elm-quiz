import Html exposing (..)
import Html.App as Html
import Html.Events exposing (..)
import Task exposing (perform,succeed)
import String
import Random
import TaskList exposing (processQueue,ItemProcessor)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL

queue = [0,1,0,3]

type alias Model = {
    taskList: TaskList.Model Int String
}

(taskListModel,taskListCmd,updateTaskList) = processQueue TaskListMsg processItem queue

init = ({taskList = taskListModel},taskListCmd)

straightAway do v = Task.perform do do (succeed v)

processItem : ItemProcessor Int String
processItem addItem item =
    case item of
      0 -> Random.generate (toString >> ((++) "x") >> addItem) (Random.int 0 10)
      _ -> straightAway addItem (toString item)

-- UPDATE

type Msg
    = TaskListMsg (TaskList.Msg String)

update : Msg -> Model -> (Model,Cmd Msg)
update msg model = case msg of
    TaskListMsg msg -> 
        let
            (taskList,cmd) = updateTaskList msg model.taskList
        in
            ({model | taskList = taskList}, cmd)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- VIEW

describeFailure fail = ""
  
view : Model -> Html Msg
view model = div [] 
  [
    (
        ul [] (List.map 
        ((Maybe.withDefault "?") >> (\x -> li [] [text x])) model.taskList.items)
    )
    , p [] [text (if model.taskList.done then "done" else "working")]
  ]

