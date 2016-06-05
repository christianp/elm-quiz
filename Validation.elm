module Validation exposing (Validation,nonEmptyValidator,modelValid)
import String

type alias Validation model = model -> Result String Bool

nonEmptyValidator field message model =
    if String.isEmpty (field model) then Err message else Ok True

modelValid model validators = 
    let
        runValidator : model -> Validation model -> Result String Bool -> Result String Bool
        runValidator model validator previous_result =
            case previous_result of
                Err message -> Err message
                Ok _ -> validator model
    in
       List.foldl (runValidator model) (Ok True) validators

