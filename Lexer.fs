type Token = string
type Expression = 
    | Var of Token
    | Abstraction of Token * Expression
    | Application of Expression * Expression
    | IntLiter of int
    | IfThenElse of Expression * Expression * Expression
    | LetIn of Token * Expression * Expression
    | Recursion of Token * Expression * Expression
    | FuncDefinition of Token
    | Operator of Token * int * Expression list
    | BracketExpr of Expression * Tree
    | RecBracketExpr of Expression * Tree * Token
and Tree = Map<Token, Expression>

let operators = 
    let binaryOperator f = function [IntLiter a; IntLiter b] -> IntLiter(f a b)
    [
        ("+", binaryOperator (+))
        ("-", binaryOperator (-))
        ("*", binaryOperator (*))
        ("/", binaryOperator (/))
        ("=", fun [IntLiter a; IntLiter b] -> if a = b then IntLiter 1 else IntLiter 0)
        (">", fun [IntLiter a; IntLiter b] -> if a > b then IntLiter 1 else IntLiter 0)
        ("<", fun [IntLiter a; IntLiter b] -> if a < b then IntLiter 1 else IntLiter 0)
        ("<=", fun [IntLiter a; IntLiter b] -> if a <= b then IntLiter 1 else IntLiter 0)
    ] |> Map.ofList

let applyOperator op args =
    match Map.tryFind op operators with
    | Some f -> f args
    | None -> failwithf "Unknown operator: %s" op

let operatorArgCount = function
    | "sin" -> 1
    | "cos" -> 1
    | "tg" -> 1
    | _ -> 2

let rec apply x y =
    match x with 
    | BracketExpr(Abstraction(v, e), env) -> calculateExpression e (Map.add v y env)
    | RecBracketExpr(Abstraction(v, e), env, id) -> calculateExpression e (Map.add v y (Map.add id x env))
    | Operator(op, n, args) when n = 1 -> applyOperator op (args @ [y])
    | Operator(op, n, args) -> Operator(op, n - 1, args @ [y])

and calculateExpression expr env =
    match expr with
    | Application(x, y) -> apply (calculateExpression x env) (calculateExpression y env)
    | IntLiter n -> IntLiter n
    | Var x -> Map.find x env
    | FuncDefinition f -> Operator(f, operatorArgCount f, [])
    | Operator(op, n, args) -> Operator(op, n, args)
    | IfThenElse(e0, e1, e2) -> 
        if calculateExpression e0 env = IntLiter 1 then calculateExpression e1 env else calculateExpression e2 env
    | LetIn(id, e1, e2) -> 
        let value = calculateExpression e1 env
        calculateExpression e2 (Map.add id value env)
    | Recursion(id, e1, e2) -> 
        calculateExpression e2 (Map.add id (RecBracketExpr(e1, env, id)) env)
    | Abstraction(_, _) -> BracketExpr(expr, env)
    | BracketExpr(expr, _) -> expr

let calculate exp = calculateExpression exp Map.empty

// Примеры использования
[ 
    Application(Application(FuncDefinition "+", IntLiter 6), IntLiter 3)
    Application(Abstraction("x", Var "x"), IntLiter 136)
    LetIn("y", IntLiter 5, Application(Abstraction("x", Application(Application(FuncDefinition "+", Var "x"), Var "y")), IntLiter 1))
    LetIn("id", Abstraction("x", Var "x"), LetIn("sq", Abstraction("z", Application(Application(FuncDefinition "*", Var "z"), Var "z")), Application(Var "sq", Application(Var "id", IntLiter 2))))
]
|> List.iter (fun expr -> printfn "Result = %A" (calculate expr))