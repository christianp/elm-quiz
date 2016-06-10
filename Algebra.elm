module Algebra exposing (Expression,TAtom,EvaluatedAtom,Name,DefinitionDict,Scope,eval,evaluateDefinitions,compileDefinitions,compile,unwrapNumber)
import Html exposing (..)
import Html.App as Html
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import String
import Dict
import Dict exposing (Dict)
import Set
import Combine exposing (..)
import Combine.Infix exposing (..)
import Combine.Num exposing (int)
import Random
import Task
import Time

type Expression
 = JustAtom TAtom
 | WaitForResult
 | BinOp TOp Expression Expression
 | UnOp TOp Expression
 | FunctionApplication TOp (List Expression)

type alias TOp = Name

type EvalMsg
    = ReceiveResult (List Int) TAtom

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
type alias EvaluatedExpression = Result EvalError Expression

eval : Scope -> Expression -> (Result EvalError Expression,Cmd EvalMsg)
eval = evalHelper []

evalHelper : List Int -> Scope -> Expression -> (Result EvalError Expression,Cmd EvalMsg)
evalHelper path scope expr = 
    let
        evalScope n = evalHelper (n::path) scope
        receiveResult = ReceiveResult path
    in
        case expr of
            WaitForResult -> (Ok WaitForResult,Cmd.none)
            JustAtom a -> (evalAtom scope a,Cmd.none)
            UnOp op a -> case evalScope 0 a of
                (Err err,msg) -> (Err err,msg)
                (Ok (JustAtom x),_) -> evalUnOp receiveResult op x
                (Ok expr,msg) -> (Ok (UnOp op expr),msg)
            BinOp op a b -> case evalScope 0 a of
                (Err err,msg) -> (Err err,msg)
                (Ok (JustAtom x),_) -> case evalScope 1 b of
                    (Err err,msg) -> (Err err,msg)
                    (Ok (JustAtom y),_) -> evalBinOp receiveResult op x y
                    (Ok expr,msg) -> (Ok (BinOp op (JustAtom x) expr),msg)
                (Ok expr,msg) -> (Ok (BinOp op expr b),msg)
            FunctionApplication op args -> (Ok (FunctionApplication op args),Cmd.none)   --evalFunction op (evalArgList (List.map evalScope args))

evalAtom : Scope -> TAtom -> EvaluatedExpression
evalAtom scope atom = case atom of
    TName name -> case Dict.get name scope of
        Nothing -> Err <| "variable "++name++" not in scope"
        Just x -> Ok (JustAtom x)
    x -> Ok (JustAtom x)

evalUnOp : (TAtom -> EvalMsg) -> TOp -> TAtom -> (EvaluatedExpression,Cmd EvalMsg)
evalUnOp receiveResult op a = case Dict.get op unOps of
    Nothing -> (Err <| "Unknown unary op "++op,Cmd.none)
    Just fn -> fn receiveResult a

(=>) = (,)
unOps = Dict.fromList 
    [
        "-" => (
            \_ -> \a -> case a of
                TNumber x -> (Ok <| JustAtom (TNumber -x),Cmd.none)
                _ -> (Err "-u something other than a number",Cmd.none)
            ),
        "!" => (\receiveResult -> \_ -> 
            let
                get = (\t -> receiveResult <| TNumber t)
            in
                (Ok WaitForResult,Task.perform get get (Time.now))
        )
    ]

evalBinOp : (TAtom -> EvalMsg) -> TOp -> TAtom -> TAtom -> (EvaluatedExpression,Cmd EvalMsg)
evalBinOp receiveResult op a b = case Dict.get op binOps of
    Nothing -> (Err <| "unknown op "++op,Cmd.none)
    Just fn -> fn receiveResult a b

evalBinNumberOp : (Float -> Float -> EvaluatedExpression) -> TAtom -> TAtom -> EvaluatedExpression
evalBinNumberOp fn ea eb = case (ea,eb) of
    (TNumber a,TNumber b) -> fn a b
    _ -> Err "op on something other than a number"

numBinOp op _ a b = (evalBinNumberOp (\a -> \b -> Ok <| JustAtom <| TNumber <| op a b) a b,Cmd.none)

binOps = Dict.fromList 
    [
        ("+",numBinOp (+)),
        ("-",numBinOp (-)),
        ("*",numBinOp (*)),
        ("/",numBinOp (/)),
        ("r",binRandom)
    ]

binRandom : (TAtom -> EvalMsg) -> TAtom -> TAtom -> (EvaluatedExpression,Cmd EvalMsg)
binRandom receiveResult aa ab = case (aa,ab) of
    (TNumber a,TNumber b) -> (Ok WaitForResult,Random.generate (\n -> receiveResult <| TNumber (toFloat n)) (Random.int (floor a) (floor b)))
    _ -> (Err "Random on something other than a number",Cmd.none)

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
    WaitForResult -> "?"
    JustAtom a -> renderAtom a
    BinOp op a b -> (maybeBracket a)++" "++op++" "++(maybeBracket b)
    UnOp op a -> op++(maybeBracket a)
    FunctionApplication fn args -> fn++"("++(String.join ", " (List.map renderExpression args))++")"

shouldBeBracketed : Expression -> Bool
shouldBeBracketed expr = case expr of
    WaitForResult -> False
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
    WaitForResult -> Set.empty
    JustAtom (TName name) -> Set.singleton name
    JustAtom _ -> Set.empty
    BinOp op a b -> Set.union (findVars a) (findVars b)
    UnOp op a -> findVars a
    FunctionApplication op args -> listUnion (List.map findVars args)

-- Evaluate a dict of definitions

type alias Scope = Dict Name TAtom
type alias DefinitionDict = Dict Name String

compileDefinitions : DefinitionDict -> Result String (Dict Name Expression)
compileDefinitions defStrings = Dict.foldl (\name -> \defString -> \out -> Result.map2 (\def -> \dict -> Dict.insert name def dict) (compile defString) out) (Ok Dict.empty) defStrings

{-
evaluateDefinitions : DefinitionDict -> Result String Scope
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
-}

type DefinitionsMsg = UpdateVariable Name EvalMsg
type alias DefinitionsModel = Result String {order: List Name, definitions: Dict Name Expression, variables: Scope}

evaluateDefinitions : DefinitionDict -> DefinitionsModel
evaluateDefinitions definitionStrings = 
    let
        rdefinitions : Result String (Dict Name Expression)
        rdefinitions = compileDefinitions definitionStrings
    in
        case rdefinitions of
            Err msg -> Err msg
            Ok definitions -> Ok {definitions = definitions, order = executionOrder definitions, variables = Dict.empty}
{-
    model is valid
    nothing to do -> unchanged
    change var ->
        var not defined -> err
        rexpr,cmd = eval var
        error evaluating -> err
        nexpr -> 
            more to do -> update var, do cmd
            finished -> chop var off order, runEvaluateDefinitions on remaining
-}

runEvaluateDefinitions : DefinitionsModel -> (DefinitionsModel,Cmd DefinitionsMsg)
runEvaluateDefinitions rmodel = case rmodel of
    Err msg -> (rmodel,Cmd.none)
    Ok model -> case model.order of
        [] -> (Ok model,Cmd.none)
        var::rest -> case Dict.get var model.definitions of
            Nothing -> (Err <| "Tried to evaluate variable "++var++", which hasn't been defined.",Cmd.none)
            Just expr ->
                let
                    (rexpr,cmd) = Debug.log "evaluate" (eval model.variables expr)
                in
                    case rexpr of
                        Err msg -> (Err <| "Error evaluating "++var++": "++msg,Cmd.none)
                        Ok nexpr -> 
                            let
                                go = ready expr
                                variables = case nexpr of
                                    JustAtom v -> Dict.insert var v model.variables
                                    _ -> model.variables
                                order = if ready nexpr then Debug.log ("finished "++var) rest else (var::rest)
                                vmodel = {model| definitions = Dict.insert var nexpr model.definitions, variables = variables, order = order}
                            in
                                if ready nexpr then runEvaluateDefinitions (Ok vmodel) else (Ok vmodel,Cmd.map (UpdateVariable var) cmd)

ready : Expression -> Bool
ready expr = case expr of
    JustAtom _ -> True
    _ -> False

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

-- work out what order a set of variables need to be evaluated in so that dependencies can be resolved
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
addop = choice [ (BinOp "+") <$ string "+", (BinOp "-") <$ string "-", (BinOp "r") <$ string "r" ]

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

unary : Parser Expression
unary = choice [(UnOp "-") <$ string "-", (UnOp "!") <$ string "!"] `Combine.andMap` factor

factor : Parser Expression
factor = rec <| \() -> between ws ws (parens expr <|> atom <|> unary)

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

type alias TestModel = 
    {
        expr: Result EvalError Expression,
        defs: DefinitionsModel
    }

type TestMsg
 = UpdateExpr EvalMsg
 | UpdateScope DefinitionsMsg
 | ReRoll

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
        ("q","1 r 10"),
        ("t","(!1-1465554950231)/1000"),
        ("y","1"),
        ("z","t+q+y"),
        ("zq","z-q-t")
    ]

def = "(!1 r 3)+(50r60)"
ex = compile def
(initExpr,initExprCmd) = case ex of
    Err err -> (Err err,Cmd.none)
    Ok x -> eval Dict.empty x

(initDefs,initDefsCmd) = runEvaluateDefinitions <| evaluateDefinitions definitions

init : (TestModel, Cmd TestMsg)
init = 
    (
        { expr = initExpr, defs = initDefs },
        Cmd.batch [Cmd.map UpdateScope initDefsCmd]
    )

view model = div []
    [
        p [] [ text def ],
        p [] [ text (Result.withDefault "?" (Result.map renderExpression ex))],
        p [] [ text (toString model.expr)],
        hr [] [],
        viewDefinitions model.defs
    ]

viewDefinitions rdefs = div []
    [
        (case rdefs of
            Err err -> p [] [text <| "Error "++err]
            Ok defs -> table [] <| Dict.foldr (\name -> \bit -> \l -> bit::l) [] (Dict.map (\name -> \value -> tr [] [ th [] [text name], td [] [text <| renderExpression (JustAtom value) ]]) defs.variables)
        ),
        p [] [text (toString rdefs)],
        button [onClick ReRoll] [text "Reroll"]
    ]

update : TestMsg -> TestModel -> (TestModel,Cmd TestMsg)
update msg model = case Debug.log "update test" msg of
    UpdateExpr emsg ->
        let
            (expr,cmd) = updateExpression emsg model.expr
        in
            ({model | expr = expr},Cmd.map UpdateExpr cmd)
    UpdateScope vmsg -> 
        let
            (defs,cmd) = updateDefinitions vmsg model.defs
        in
            ({model|defs = defs},Cmd.map UpdateScope cmd)

    ReRoll ->
        let
            (defs,cmd) = runEvaluateDefinitions <| evaluateDefinitions definitions
        in
            ({model | defs = defs}, Cmd.map UpdateScope cmd)

updateDefinitions : DefinitionsMsg -> DefinitionsModel -> (DefinitionsModel, Cmd DefinitionsMsg)
updateDefinitions msg rmodel = case Debug.log "Update definitions" msg of
    UpdateVariable name vmsg -> case rmodel of
        Err emsg -> (Err emsg,Cmd.none)
        Ok model -> case Dict.get name model.definitions of
            Nothing -> (Err <| "Tried to update variable "++name++", which hasn't been defined",Cmd.none)
            Just expr -> 
                let
                    (rexpr,cmd) = updateExpression vmsg (Ok expr)
                in
                    case rexpr of
                        Err emsg -> (Err <| "Error evaluating "++name++": "++emsg,Cmd.none)
                        Ok nexpr -> 
                            let
                                go = ready nexpr
                                nmodel = {model | definitions = Dict.insert name nexpr model.definitions}
                            in
                                if ready nexpr then
                                    runEvaluateDefinitions (Ok nmodel)
                                else
                                    (Ok nmodel,Cmd.map (UpdateVariable name) cmd)

updateExpression : EvalMsg -> Result EvalError Expression -> (Result EvalError Expression,Cmd EvalMsg)
updateExpression msg model = case Debug.log "update expr" msg of
    ReceiveResult path x -> case model `Result.andThen` (placeResult path x) of
        Err err -> (Err err,Cmd.none)
        Ok expr -> eval Dict.empty expr

placeResult: List Int -> TAtom -> Expression -> Result String Expression
placeResult path atom expr = case Debug.log "place" path of
    [] -> case expr of
        WaitForResult -> Ok <| JustAtom atom
        _ -> Err <| "Trying to place a result at a "++(toString expr)
    a::rest -> case (expr,a) of
        (WaitForResult,_) -> Err "Still got a path but reached a terminal"
        (JustAtom _,_) -> Err "Still got a path but reached a terminal"
        (UnOp op subexpr,0) -> placeResult rest atom subexpr `Result.andThen` (\r -> Ok <| UnOp op r)
        (UnOp _ _,_) -> Err "Path > 0 when placing through UnOp"
        (BinOp op a b,0) -> placeResult rest atom a `Result.andThen` (\sa -> Ok <| BinOp op sa b)
        (BinOp op a b,1) -> placeResult rest atom b `Result.andThen` (\sb -> Ok <| BinOp op a sb)
        (BinOp _ _ _,_) -> Err "Path > 1 when placing through BinOp"
        _ -> Err "Not implemented"


subscriptions model = Sub.none
