module Algebra exposing (Expression,TAtom,eval)
import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import String
import Dict
import Set

type Expression
 = JustAtom TAtom
 | BinOp TOp Expression Expression
 | UnOp TOp Expression
 | FunctionApplication TOp (List Expression)

type alias TOp = Name

type TAtom
 = TNumber Float
 | TString String
 | TName Name

type alias Name = String

type alias EvalError = String

type alias EvaluatedAtom = Result EvalError TAtom

eval : Expression -> EvaluatedAtom
eval expr = case expr of
    JustAtom a -> Ok a
    BinOp op a b -> evalBinOp op (eval a) (eval b)
    UnOp op a -> evalUnOp op (eval a)
    FunctionApplication op args -> evalFunction op (evalArgList (List.map eval args))

evalBinNumberOp : (Float -> Float -> EvaluatedAtom) -> TAtom -> TAtom -> EvaluatedAtom
evalBinNumberOp fn ea eb = case (ea,eb) of
    (TNumber a,TNumber b) -> fn a b
    _ -> Err "op on something other than a number"

numBinOp op = evalBinNumberOp (\a -> \b -> Ok (TNumber (op a b)))

binOps = Dict.fromList 
    [
        ("+",numBinOp (+)),
        ("-",numBinOp (-)),
        ("*",numBinOp (*)),
        ("/",numBinOp (/))
    ]

evalBinOp : TOp -> EvaluatedAtom -> EvaluatedAtom -> EvaluatedAtom
evalBinOp op ea eb = case (ea,eb) of
    (Ok a, Ok b) -> case Dict.get op binOps of
        Nothing -> Err <| "unknown op "++op
        Just fn -> fn a b
    (Err err,_) -> Err <| "Error in LHS: "++err
    (Ok a,Err err) -> Err <| "Error in RHS: "++err

evalUnOp : TOp -> EvaluatedAtom -> EvaluatedAtom
evalUnOp op ea = case ea of
    Ok a -> case op of
        "-" -> case a of
            TNumber x -> Ok (TNumber -x)
            _ -> Err "-u something other than a number"
        _ -> Err <| "unknown op "++op
    Err err -> Err err

evalFunction : TOp -> Result EvalError (List TAtom) -> EvaluatedAtom
evalFunction op eargs = case eargs of
    Ok args -> case op of
        "cos" -> fnCos args
        _ -> Err <| "unknown op "++op
    Err msg -> Err msg

fnCos args = 
    let
        n = List.head args
    in
        case n of
            Nothing -> Err "no argument given"
            Just (TNumber x) -> Ok (TNumber (cos x))
            _ -> Err "sin something other than a number"

evalArgList : List EvaluatedAtom -> Result EvalError (List TAtom)
evalArgList args = case args of
    [] -> Ok []
    a::rest -> case a of
        Ok x ->
            let
                eRest = evalArgList rest
            in
                case eRest of
                    Ok l -> Ok (x::l)
                    Err msg -> Err msg
        Err msg -> Err msg

renderExpression : Expression -> String
renderExpression expr = case expr of
    JustAtom a -> renderAtom a
    BinOp op a b -> (maybeBracket a)++op++(maybeBracket b)
    UnOp op a -> (maybeBracket a)++op
    FunctionApplication fn args -> fn++"("++(String.join ", " (List.map renderExpression args))++")"

shouldBeBracketed : Expression -> Bool
shouldBeBracketed expr = case expr of
    JustAtom _ -> False
    BinOp _ _ _ -> True
    UnOp _ _ -> True
    FunctionApplication _ _ -> False

maybeBracket : Expression -> String
maybeBracket expr =
    let
        inside = renderExpression expr
    in
        if shouldBeBracketed expr then "("++inside++")" else inside

renderAtom : TAtom -> String
renderAtom a = case a of
    TNumber n -> if n==pi then "pi" else toString n
    TString s -> s
    TName name -> name

listUnion : List (Set.Set comparable) -> Set.Set comparable
listUnion sets = case sets of
    [] -> Set.empty
    a::rest -> Set.union a (listUnion rest)

findVars : Expression -> Set.Set Name
findVars expr = case expr of
    JustAtom (TName name) -> Set.singleton name
    JustAtom _ -> Set.empty
    BinOp op a b -> Set.union (findVars a) (findVars b)
    UnOp op a -> findVars a
    FunctionApplication op args -> listUnion (List.map findVars args)

-- demo
 
main = Html.program 
    {
        init = init,
        view = view,
        update = update,
        subscriptions = subscriptions
    }

type Msg
    = NoOp

(!+) = \a -> \b -> BinOp "+" a b
(!-) = \a -> \b -> BinOp "-" a b
(!*) = \a -> \b -> BinOp "*" a b
(!/) = \a -> \b -> BinOp "/" a b
n x = JustAtom (TNumber x)

fa = FunctionApplication

expr = fa "cos" [ (n pi) !/ (n 1 !* n 2)]

expr2 = (fa "cos" [JustAtom (TName "x")]) !+ (JustAtom (TName "x")) !+ (JustAtom (TName "y"))

showResult: EvaluatedAtom -> String
showResult res = case res of
    Err msg -> "ERROR: "++msg
    Ok a -> renderAtom a

init = (expr,Cmd.none)
view model = div [] 
    [
        p [] [text (toString (findVars expr2))],
        p [] [text (renderExpression model)],
        text (showResult (eval model))
    ]

update msg model = (model,Cmd.none)
subscriptions model = Sub.none
