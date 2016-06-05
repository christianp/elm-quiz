module Input exposing (formInput,textInput,passwordInput)
import Html exposing (..)
import Html.Attributes exposing (..)

formInput : String -> String -> List (Attribute x) -> Html x
formInput t p attr = input ([type' t, placeholder p] ++ attr) []
textInput = formInput "text"
passwordInput = formInput "password"

