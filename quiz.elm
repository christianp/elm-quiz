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
import Algebra exposing (..)
import Dict
import Platform.Cmd exposing (Cmd)

main =
    Html.program
    {
        init = init,
        view = view,
        update = update,
        subscriptions = subscriptions
    }

subscriptions model = Sub.none


-- MODEL

type alias QuestionPager = Pager.Model Question.Model Question.Msg

type alias Model = 
    {
        title: String,
        pager: Maybe QuestionPager,
        variableDefinitions : DefinitionDict,
        questionDefinitions : List (Scope -> Question.Model),
        scope : Maybe (Result String Scope)
    }

type alias IndexedPart = IndexedItem Part.Model

emptyModel : Model
emptyModel = {title="", pager = Nothing, variableDefinitions = Dict.empty, scope = Nothing, questionDefinitions = []}

questionDefinitions = 
    [
        Question.singlePartQuestion <| Part.newPart "x" (Part.numberPart "x-1" "x+1"),
        Question.singlePartQuestion <| Part.newPart "Write an odd number of x" (Part.regexPart "^x(xx)*$" "xxx"),
        Question.singlePartQuestion <| Part.newPart "Write x" (Part.stringPart "x"),
        Question.multiPartQuestion
            [
                Part.newPart "a" (Part.stringPart "a"),
                Part.newPart "one digit" (Part.numberPart "0" "9")
            ]
    ]

{-
questionDefinitions =
    [
        Question.singlePartQuestion <| Part.newPart "Write an odd number of x" (Part.regexPart "^x(xx)*$" "xxx")
    ]
-}

scope = Dict.empty

init = 
    (
        { emptyModel | 
            title="Test quiz", 
            questionDefinitions = questionDefinitions,
            variableDefinitions = Dict.fromList [("x","1")]
        },
        Cmd.none
    )

nth : Int -> List x -> Maybe x
nth n list = List.head (List.drop n list)

-- UPDATE

type Msg
    = NextQuestion
    | PrevQuestion
    | UpdatePager (Pager.Msg Question.Msg)
    | GenerateVariables

update : Msg -> Model -> (Model,Cmd Msg)
update msg model = (updateModel msg model,Cmd.none)

updateModel action model =
    case action of
        PrevQuestion -> updatePager action model
        NextQuestion -> updatePager action model
        UpdatePager msg -> updatePager action model
        GenerateVariables ->
            let
                scope = evaluateDefinitions model.variableDefinitions
                questions = case scope of
                    Err msg -> Nothing
                    Ok s -> Just (List.map (\q -> q s) model.questionDefinitions)
                pager = Maybe.map (\questions -> Pager.init Question.update Question.view questions) questions
            in
                {model | scope=Just scope,pager=pager }


updatePager : Msg -> Model -> Model
updatePager action model = case model.pager of
    Nothing -> model
    Just pager -> case action of
        PrevQuestion -> {model | pager = Just (Pager.update Pager.doPrevItem pager)}
        NextQuestion -> {model | pager = Just (Pager.update Pager.doNextItem pager)}
        UpdatePager msg -> {model | pager = Just (Pager.update msg pager)}
        _ -> model
-- VIEW

numberCorrect pager = List.length (List.filter (\x -> Question.markQuestion x.item) pager.items)

currentQuestionNumber : QuestionPager -> String
currentQuestionNumber pager = 
    case pager.current of
        Nothing -> ""
        Just x -> toString (x+1)

numQuestions pager= List.length pager.items

view : Model -> Html Msg
view model = case model.scope of
    Nothing -> viewFrontPage model
    Just (Err msg) -> div [] [text msg]
    Just (Ok scope) -> case model.pager of
        Nothing -> div [] [text "No pager"]
        Just pager -> viewQuiz model pager

viewFrontPage model =
    div [] [button [onClick GenerateVariables] [text "Generate"]]

viewQuiz : Model -> QuestionPager -> Html Msg
viewQuiz model pager = 
    div [class "quiz"] [
        header [class "quiz-header"] 
            [
              h1 [class "quiz-title"] [text model.title]
            , div [class "quiz-feedback"] [text ((toString (numberCorrect pager))++" correct")]
            , p [class "question-number"] [text ("Question "++(currentQuestionNumber pager)++"/"++(toString (numQuestions pager)))]
            , nav [class "question-nav"] [button [onClick PrevQuestion] [text "PREV"], button [onClick NextQuestion] [text "NEXT"]]
            ]
        , Html.map UpdatePager (Pager.view pager)
        , p [] [text (toString model.scope)]
    ]
