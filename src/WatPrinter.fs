module Fado.Core.WatPrinter

open Fado.Core.WatAst

let rec printExpr (expr: Expr) =
    match expr with
    | Operation(kind, tags, typ) ->
        match kind with
        | Binary(operator, left, right) ->
            match operator with
            | BinaryPlus ->
                let leftStr = printExpr left
                let rightStr = printExpr right
                sprintf "%s %s %s" leftStr "+" rightStr
            | _ -> ""
        | _ -> ""
    | Value(kind) ->
        match kind with
        | NumberConstant(value) ->
            match value with
            | NumberValue.Int32 i32 -> i32.ToString()
            | NumberValue.Int64 i64 -> i64.ToString()
        | _ -> ""
    | _ -> ""

let rec printDeclaration (decl: WatAst.Declaration) =
    match decl with
    | ModuleDeclaration modDecl ->
        printDeclarations modDecl.Members
    | MemberDeclaration memDecl ->
        let exprStr = printExpr memDecl.Body
        sprintf "%s = %s" memDecl.Name exprStr

and printDeclarations (decls: WatAst.Declaration list) =
    let wat =
        [ for decl in decls -> 
            printDeclaration decl ]
        |> Seq.reduce (+)
    wat

let printAst (decls : WatAst.Declaration list) =
    let wat = printDeclarations decls
    wat