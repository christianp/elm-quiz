module Part exposing (Model,newPart,stringPart,numberPart,Msg,update,view,makeSubmit,markPart)
import Html exposing (..)
import String
import List exposing (map)
import Html.Attributes exposing (..)
import Html.Lazy exposing (lazy)
import Html.Events exposing (onInput, onClick, onSubmit)
import Validation exposing (..)
import Input exposing (..)

-- models

type alias StringInfo = {correctAnswer: String}
type alias NumberInfo = {minAnswer: Int, maxAnswer: Int}

type Marker 
    = EmptyMarker
    | StringMarker StringInfo
    | NumberMarker NumberInfo

type alias EmptyAnswer = ()

type Answer
    = EmptyAnswer
    | StringAnswer String
    | NumberAnswer String

type alias Model =
    { 
        marker: Marker,
        stagedAnswer: Answer,
        submittedAnswer: Maybe Answer,
        prompt: String
    }

newPart : String -> Model
newPart prompt = {prompt = prompt, marker = EmptyMarker, stagedAnswer = EmptyAnswer, submittedAnswer = Nothing}

stringPart : Model -> String -> Model
stringPart model correctAnswer = {model | marker = StringMarker {correctAnswer = correctAnswer}, stagedAnswer = StringAnswer ""}

numberPart : Model -> Int -> Int -> Model
numberPart model minAnswer maxAnswer = {model | marker = NumberMarker {minAnswer = minAnswer, maxAnswer = maxAnswer}, stagedAnswer = NumberAnswer ""}

-- update

type Msg
    = StagedAnswer Answer
    | Submit

makeSubmit : Msg
makeSubmit = Submit

update : Msg -> Model -> Model
update action qn =
    case action of
        StagedAnswer answer -> {qn | stagedAnswer = answer}
        Submit -> {qn | submittedAnswer = Just qn.stagedAnswer}

-- validators

answerChanged : Answer -> Bool
answerChanged stagedAnswer = case stagedAnswer of
    EmptyAnswer -> False
    StringAnswer stagedAnswer -> stagedAnswer /= ""
    NumberAnswer stagedAnswer -> stagedAnswer /= ""

submittedAnswerValidator part = 
    case part.submittedAnswer of
        Nothing -> if answerChanged part.stagedAnswer then Err "Unsubmitted change" else Err "Nothing submitted"
        Just answer -> if answer==part.stagedAnswer then Ok True else Err "Unsubmitted change"

isIntValidator part = 
    case part.submittedAnswer of
        Nothing -> Err ""
        Just submittedAnswer -> 
            case submittedAnswer of 
                NumberAnswer n -> case (String.toInt n) of
                    Ok _ -> Ok True
                    Err _ -> Err "Not an integer"
                _ -> Err "Not a number answer"

partValidators : Model -> List (Validation Model)
partValidators part = [] ++ (
    case part.marker of 
        EmptyMarker -> []
        StringMarker _ -> []
        NumberMarker _ -> [isIntValidator]
    )

markPart : Model -> Bool
markPart part = case part.submittedAnswer of
    Nothing -> False
    Just answer -> case part.marker of
        EmptyMarker -> False
        StringMarker info -> case answer of
            StringAnswer answer -> markString info answer
            _ -> False
        NumberMarker info -> case answer of
            NumberAnswer answer -> 
               case String.toInt answer of
                   Err _ -> False
                   Ok n -> markNumber info n
            _ -> False

markString : StringInfo -> String -> Bool
markString {correctAnswer} answer = answer==correctAnswer
markNumber : NumberInfo -> Int -> Bool
markNumber {minAnswer,maxAnswer} n = n>=minAnswer && n<=maxAnswer

partValid : Model -> Result String Bool
partValid part = modelValid part (partValidators part)

-- views

stageStringAnswer : String -> Msg
stageStringAnswer answer = StagedAnswer (StringAnswer answer)

stageNumberAnswer : String -> Msg
stageNumberAnswer answer = StagedAnswer (NumberAnswer answer)

answerField part stage = 
    let
        (classes,validationMessage) = viewValidation part
    in
        [p [classList classes] [textInput "Answer" [onInput stage], text " ", validationMessage]]

view : Model -> Html Msg
view part = lazy (
    \part ->
        Html.div []
        [
            Html.form [onSubmit Submit]
            ([p [] [text part.prompt]]
            ++(case part.marker of
                    EmptyMarker -> []
                    StringMarker _ -> answerField part stageStringAnswer
                    NumberMarker _ -> answerField part stageNumberAnswer
            )
            ++[partFeedback part]
            ++(case part.marker of
                    EmptyMarker -> []
                    _ -> [p [] [button [] [text "Submit"]]]
            ))
        ]
    ) 
    part

partFeedback : Model -> Html Msg
partFeedback model = 
    let
        submitted = case submittedAnswerValidator model of
            Ok _ -> True
            Err _ -> False
        correct = case partValid model of
            Ok _ -> 
                Ok (markPart model)
            Err _ -> Err "Not valid"
        message = if not submitted then [] else case correct of
            Err _ -> []
            Ok result -> [text (if result then "Correct" else "Wrong")]
    in
        p [classList ([("part-feedback",True),("correct",submitted && (correct==Ok True)),("incorrect",submitted && (correct==Ok False)),("unsubmitted",not submitted)])] message

viewValidation : Model -> (List (String,Bool),Html Msg)
viewValidation model =
    let
        result = partValid model
        (valid, message) =
        case result of 
                Ok _ -> (True,"Valid")
                Err message -> (False,message)
    in
         ([("valid",valid),("invalid",not valid)], span [] [ text message ])

