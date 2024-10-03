open FParsec

type Identifier = string

type Expression =
    | Var of Identifier
    | Abstraction of Identifier * Expression
    | Application of Expression * Expression
    | IntLiteral of int
    | IfThenElse of Expression * Expression * Expression
    | LetIn of Identifier * Expression * Expression
    | Recursion of Identifier * Expression * Expression
    | FuncDefinition of Identifier
    | Operator of Identifier * int * Expression list
    | BracketExpr of Expression * Map<Identifier, Expression>
    | RecBracketExpr of Expression * Map<Identifier, Expression> * Identifier

let ws = spaces
let str_ws s = pstring s .>> ws
let parenthesized p = between (str_ws "(") (str_ws ")") p

let pIdentifier = many1Satisfy2 isAlphaNum |>> String.concat "" .>> ws
let pIntLiteral = pint32 |>> IntLiteral

let pVariable = pIdentifier |>> Var

let pAbstraction = pipe3 (str_ws "\\" >>. pIdentifier) (str_ws "->") pExpression Abstraction

let pApplication = 
    pipe2 (choice [pAbstraction; pVariable; pIntLiteral; parenthesized pExpression])
          (choice [pAbstraction; pVariable; pIntLiteral; parenthesized pExpression])
          Application

let pIfThenElse = 
    pipe3 (str_ws "if" >>. pExpression)
          (str_ws "then" >>. pExpression)
          (str_ws "else" >>. pExpression)
          IfThenElse

let pLetIn =
    pipe3 (str_ws "let" >>. pIdentifier .>> str_ws "=" >>. pExpression)
          (str_ws "in" >>. pExpression)
          LetIn

let pRecursion =
    pipe3 (str_ws "let rec" >>. pIdentifier .>> str_ws "=" >>. pAbstraction)
          (str_ws "in" >>. pExpression)
          Recursion

let pFuncDefinition = pIdentifier |>> FuncDefinition

let pOperator = pipe3 pFuncDefinition pint32 (sepBy pExpression ws) Operator

let pBracketExpr =
    pipe2 (str_ws "{" >>. pExpression) 
          (str_ws "}" >>. many (pIdentifier .>> str_ws "=" >>. pExpression) |>> Map.ofList) 
          BracketExpr

let pRecBracketExpr =
    pipe3 (str_ws "{" >>. str_ws "rec" >>. pIdentifier .>> str_ws "=" >>. pAbstraction)
          (str_ws "}" >>. many (pIdentifier .>> str_ws "=" >>. pExpression) |>> Map.ofList)
          RecBracketExpr

let pExpression = choice [
    pApplication
    pIntLiteral
    pVariable
    pIfThenElse
    pLetIn
    pRecursion
    pOperator
    parenthesized pExpression
    pBracketExpr
    pRecBracketExpr
]

let parse str =
    match run pExpression str with
    | Success(result, _, _) -> result
    | Failure(errorMsg, _, _) -> failwith errorMsg

