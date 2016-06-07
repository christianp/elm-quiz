module Algebra exposing (Expression,TAtom,EvaluatedAtom,Name,DefinitionDict,Scope,eval,evaluateDefinitions,compileDefinitions,compile,unwrapNumber)
import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import String
import Dict
import Dict exposing (Dict)
import Set
import Combine exposing (..)
import Combine.Infix exposing (..)
import Combine.Num exposing (int)

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

unwrapNumber v = case v of
    TNumber n -> Ok n
    _ -> Err "not a number"

type alias Name = String

type alias EvalError = String

type alias EvaluatedAtom = Result EvalError TAtom

eval : Scope -> Expression -> EvaluatedAtom
eval scope expr = 
    let
        evalScope = eval scope
    in
        case expr of
            JustAtom a -> evalAtom scope a
            BinOp op a b -> evalBinOp op (evalScope a) (evalScope b)
            UnOp op a -> evalUnOp op (evalScope a)
            FunctionApplication op args -> evalFunction op (evalArgList (List.map evalScope args))

evalAtom scope atom = case atom of
    TName name -> case Dict.get name scope of
        Nothing -> Err <| "variable "++name++" not in scope"
        Just x -> Ok x
    x -> Ok x

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

-- render an expression to a string

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

-- find the variables used in an expression

findVars : Expression -> Set.Set Name
findVars expr = case expr of
    JustAtom (TName name) -> Set.singleton name
    JustAtom _ -> Set.empty
    BinOp op a b -> Set.union (findVars a) (findVars b)
    UnOp op a -> findVars a
    FunctionApplication op args -> listUnion (List.map findVars args)

-- Evaluate a dict of definitions

type alias DefinitionDict = Dict Name String
type alias Scope = Dict Name TAtom

compileDefinitions : DefinitionDict -> Result String (Dict Name Expression)
compileDefinitions defStrings = Dict.foldl (\name -> \defString -> \out -> Result.map2 (\def -> \dict -> Dict.insert name def dict) (compile defString) out) (Ok Dict.empty) defStrings

evaluateDefinitions : DefinitionDict-> Result String Scope
evaluateDefinitions definitionStrings = 
    let
        rdefinitions = compileDefinitions definitionStrings
    in
        case rdefinitions of
            Err msg -> Err <| "When compiling definitions: "++msg
            Ok definitions ->
                let
                    order = executionOrder definitions
                    doVar : Name -> Result String Scope -> Result String Scope
                    doVar var rscope = 
                        case rscope of
                            (Err msg) as err -> err
                            Ok scope -> case Dict.get var definitions of
                                Nothing -> rscope
                                Just def -> 
                                    case eval scope def of
                                        Err msg -> Err <| "When evaluating "++var++": "++msg
                                        Ok v -> Ok (Dict.insert var v scope)
                in
                    List.foldl doVar (Ok Dict.empty) order

dependencies: Dict Name Expression -> Dict Name (Set.Set Name)
dependencies defs = Dict.map (\_ -> \expr -> findVars expr) defs

validDefinition : Set.Set Name -> Dict Name (Set.Set Name) -> Name -> Result String ()
validDefinition seen deps var = case Dict.get var deps of
    Nothing -> Err <| "Variable "++var++" not defined"
    Just vars -> 
        if Set.member var seen then 
            Err <| "cycle involving "++var 
        else if Set.isEmpty vars then Ok () else
            Set.foldr 
                (\d -> \r -> Result.andThen r (\_ -> validDefinition (Set.insert var seen) deps d)) 
                (Ok ()) 
                vars

validGraph : Dict Name Expression -> Result String ()
validGraph definitions = 
    let
        deps = dependencies definitions
        varOk name = case validDefinition Set.empty deps name of
            Err msg -> Err <| "In definition of "++name++": "++msg
            Ok () -> Ok ()
    in
        List.foldr (\d -> \r -> Result.andThen r (\_ -> varOk d)) (Ok ()) (Dict.keys definitions)

executionOrder : Dict Name Expression -> List Name
executionOrder definitions =
    let
        deps = dependencies definitions
        names = Dict.keys deps
    in
        executionOrderHelp [] deps names

executionOrderHelp : List Name -> Dict Name (Set.Set Name) -> List Name -> List Name
executionOrderHelp seen deps vars = case vars of
    [] -> []
    a::rest ->
        let
            aDeps = case Dict.get a deps of
                Nothing -> []
                Just x -> Set.toList x
            myOrder = List.foldl (\b -> \seen2 -> mergeLists seen2 (executionOrderHelp seen deps [b])) seen aDeps
            nseen = myOrder ++ [a]
        in
            mergeLists nseen (executionOrderHelp nseen deps rest)

mergeLists : List a -> List a -> List a
mergeLists a b = case b of
    [] -> a
    x::rest -> mergeLists (if List.member x a then a else a++[x]) rest

-- parser

ws : Parser String
ws = regex "[ \t\r\n]*"

addop : Parser (Expression -> Expression -> Expression)
addop = choice [ (BinOp "+") <$ string "+", (BinOp "-") <$ string "-" ]

mulop : Parser (Expression -> Expression -> Expression)
mulop = choice [ (BinOp "*") <$ string "*", (BinOp "/") <$ string "/" ]

expr : Parser Expression
expr = rec <| \() -> term `chainl` addop

term : Parser Expression
term = rec <| \() -> factor `chainl` mulop

parseTNumber : Parser TAtom
parseTNumber = Combine.map (toFloat >> TNumber) int

parseName : Parser Name
parseName = (regex "[a-z]+")

parseTName : Parser TAtom
parseTName = Combine.map TName parseName

parseTString : Parser TAtom
parseTString = Combine.map TString (regex "\"[^\"]*\"")

atom : Parser Expression
atom = Combine.map JustAtom (choice [parseTNumber,parseTName,parseTString])

factor : Parser Expression
factor = rec <| \() -> between ws ws (parens expr <|> atom)

compile : String -> Result String Expression
compile s =
  case parse (expr <* end) s of
    (Ok n, _) ->
      Ok n

    (Err ms, cx) ->
      Err ("parse error: " ++ (toString ms) ++ ", " ++ (toString cx))

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

showResult: EvaluatedAtom -> String
showResult res = case res of
    Err msg -> "ERROR: "++msg
    Ok a -> renderAtom a

scope = Dict.fromList 
    [
        ("x",TNumber 1),
        ("y",TNumber 2)
    ]

definitions = Dict.fromList
    [
        ("q","1"),
        ("x","q+1"),
        ("y","x+1"),
        ("z","x+y")
    ]

init = ((),Cmd.none)
view model = div [] 
    [
        p [] [
            text (case (Result.map2 (\expr -> \scope -> (renderExpression expr)++" = "++(toString (eval scope expr))) (compile "3-x") (evaluateDefinitions definitions)) of
                Err msg -> msg
                Ok s -> s
            )
        ],
        p [] [text (toString (Result.map (Dict.map (\x -> renderExpression)) (compileDefinitions definitions)))],
        p [] [text (toString (evaluateDefinitions definitions))]
    ]

update msg model = (model,Cmd.none)
subscriptions model = Sub.none
