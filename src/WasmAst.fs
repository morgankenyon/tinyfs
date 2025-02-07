module Fado.Core.WasmAst

type Literal =
    | IntLiteral of int32
    | LongLiteral of int64

type Identifier =
    {
        Name : string
        Literal : Literal
    }
type Expression =
    | IdentExpr of ident: Identifier
    | Value of literal: Literal
//type BinOp = | Plus

let printLiteral (lit: Literal) =
    match lit with
    | IntLiteral inn -> sprintf "%d" inn
    | LongLiteral longg -> sprintf "%d" longg

let printExpression (expr: Expression) =
    match expr with
    | IdentExpr ident ->
        let identStr = printLiteral ident.Literal
        sprintf "%s = %s" ident.Name identStr
    | Value literal ->
        let identStr = printLiteral literal
        identStr