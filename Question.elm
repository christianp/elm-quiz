module Question exposing (Model,singlePartQuestion,multiPartQuestion,Msg,update,view,markQuestion)
import Part exposing (..)
import List exposing (map)
import IndexedItem exposing (..)
import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick, onSubmit)
import Algebra exposing (Scope)

type Model
    = SinglePartQuestion Part.Model
    | NestedQuestion (IndexedList Model)

singlePartQuestion : (Scope -> Part.Model) -> Scope -> Model
singlePartQuestion part scope = SinglePartQuestion (part scope)
multiPartQuestion : List (Scope -> Part.Model) -> Scope -> Model
multiPartQuestion parts scope = NestedQuestion (List.indexedMap (\id -> \x -> indexItem id ((singlePartQuestion x) scope)) parts)

type Msg
    = UpdatePart Part.Msg
    | Submit
    | UpdateQuestion ItemId Msg

update : Msg -> Model -> Model
update action model =
    case model of
        SinglePartQuestion part -> case action of
            UpdatePart msg -> SinglePartQuestion (Part.update msg part)
            Submit -> SinglePartQuestion (Part.update Part.makeSubmit part)
            _ -> model
        NestedQuestion questions -> case action of
            UpdateQuestion id msg -> NestedQuestion (listUpdateItem update id msg questions)
            Submit -> NestedQuestion (map (\q -> {q | item = (update Submit q.item)}) questions)
            _ -> model

markQuestion: Model -> Bool
markQuestion question = case question of
    SinglePartQuestion part -> markPart part
    NestedQuestion questions -> List.all (\q -> markQuestion q.item) questions

questionFeedback : Model -> Html Msg
questionFeedback question = 
    let
        correct = markQuestion question
    in
        div [class ("question-feedback "++(if correct then "correct" else "incorrect"))] 
        [
            p [] [text (if correct then "Question correct" else "Question not yet correct")]
        ]

viewPart : Part.Model -> Html Msg
viewPart part = Html.map UpdatePart (Part.view part)

viewSubQuestion : Model -> Html Msg
viewSubQuestion question = case question of
    SinglePartQuestion part ->
        div [class "question single-part"] [viewPart part]
    NestedQuestion questions -> 
        div [class "question multi-part"] 
        [
            ul [class "parts"] (map (\p -> li [] [div [] [text ("Part "++(toString (p.id+1))), Html.map (UpdateQuestion p.id) (viewSubQuestion p.item)]]) questions)
            , p [] [button [onClick Submit] [text "Submit all"]]
        ]

view : Model -> Html Msg
view question = 
    div [class "question"] [
          questionFeedback question
        , viewSubQuestion question
    ]
