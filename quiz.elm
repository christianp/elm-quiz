import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick, onSubmit)
import String
import List exposing (map)
import Part
import IndexedItem exposing (..)
import Question
import Pager exposing (..)

main =
    Html.beginnerProgram
    {
        model = model
        , view = view
        , update = update
    }



-- MODEL

type alias Model = 
    {
        pager: Pager.Model Question.Model Question.Msg
    }

type alias IndexedPart = IndexedItem Part.Model

emptyModel : Model
emptyModel = {pager = Pager.init Question.update Question.view []}

questions = 
    [
        Question.singlePartQuestion (Part.stringPart (Part.newPart "Write x") "x"),
        Question.multiPartQuestion
            [
                Part.stringPart (Part.newPart "a") "a",
                Part.numberPart (Part.newPart "one digit") 0 9
            ]
    ]
model = {emptyModel | pager = Pager.init Question.update Question.view questions}

nth : Int -> List x -> Maybe x
nth n list = List.head (List.drop n list)

-- UPDATE

type Msg
    = NextQuestion
    | PrevQuestion
    | UpdatePager (Pager.Msg Question.Msg)

update : Msg -> Model -> Model
update action model =
    case action of
        PrevQuestion -> {model | pager = Pager.update Pager.doPrevItem model.pager}
        NextQuestion -> {model | pager = Pager.update Pager.doNextItem model.pager}
        UpdatePager msg -> {model | pager = Pager.update msg model.pager}

updatePart = updateItem Part.update

-- VIEW

numberCorrect model = List.length (List.filter (\x -> Question.markQuestion x.item) model.pager.items)

currentQuestionNumber model = 
    case model.pager.current of
        Nothing -> ""
        Just x -> toString (x+1)

numQuestions model = List.length model.pager.items

view : Model -> Html Msg
view model = 
    div [class "quiz"] [
        header [class "quiz-header"] 
            [
              div [class "quiz-feedback"] [text ((toString (numberCorrect model))++" correct")]
            , p [class "question-number"] [text ("Question "++(currentQuestionNumber model)++"/"++(toString (numQuestions model)))]
            , nav [class "question-nav"] [button [onClick PrevQuestion] [text "PREV"], button [onClick NextQuestion] [text "NEXT"]]
            ]
        , Html.map UpdatePager (Pager.view model.pager)
    ]
