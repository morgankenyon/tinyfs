module Fado.Core.TypedWalkthrough

open FSharp.Compiler.Text
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Syntax
open FSharp.Compiler.Symbols

// Create checker
let checker = FSharpChecker.Create()

// Sample code
let sourceCode = """
module MyModule

let add x y = x + y

type Person = { Name: string; Age: int }
let bob = { Name = "Bob"; Age = 25 }
"""

// Parse and check
let sourceText = SourceText.ofString sourceCode
let filename = "script.fsx"
let opts =
    {
    FSharpParsingOptions.Default with
        SourceFiles = [| filename |]
    }
let (projectOptions, _) = 
    checker.GetProjectOptionsFromScript(
        "script.fsx",
        sourceText)
    |> Async.RunSynchronously

let parseResults = 
    checker.ParseFile(
        "script.fsx",
        sourceText,
        opts)
    |> Async.RunSynchronously

let checkResults = 
    checker.CheckFileInProject(
        parseResults,
        "script.fsx",
        0,
        sourceText,
        projectOptions)
    |> Async.RunSynchronously

// Walk through untyped AST (from parseResults)
let rec visitExpr depth expr =
    let indent = String.replicate depth "  "
    match expr with
    | SynExpr.App(_, _, funcExpr, argExpr, _) ->
        printfn "%sFunction Application" indent
        visitExpr (depth + 1) funcExpr
        visitExpr (depth + 1) argExpr
        
    | SynExpr.Ident ident ->
        printfn "%sIdentifier: %s" indent ident.idText
        
    | SynExpr.Const(constant, _) ->
        printfn "%sConstant: %A" indent constant
        
    | SynExpr.Record(_, _, fields, _) ->
        printfn "%sRecord Creation" indent
        for field in fields do
            match field with
            | SynExprRecordField(fieldName, _, exprOption, _) ->
                match exprOption with
                | Some expr -> visitExpr (depth + 1) expr
                | None -> ()
            
    | _ -> 
        printfn "%sOther Expression: %A" indent expr

let rec visitPattern depth pat =
    let indent = String.replicate depth "  "
    match pat with
    | SynPat.Named(ident, _, _, _) ->
        match ident with
        | SynIdent(ide, trivia) ->
            printfn "%sNamed Pattern: %s" indent ide.idText
        
    | SynPat.LongIdent(LongIdentWithDots(ident, _), _, _, _, _, _) ->
        printfn "%sLong Identifier Pattern: %s" indent 
            (ident |> List.map (fun i -> i.idText) |> String.concat ".")
            
    | _ ->
        printfn "%sOther Pattern: %A" indent pat

let visitBinding depth (binding: SynBinding) =
    let indent = String.replicate depth "  "
    printfn "%sBinding:" indent
    visitPattern (depth + 1) binding.Patterns.Head
    visitExpr (depth + 1) binding.Expression

let visitDecl depth decl =
    let indent = String.replicate depth "  "
    match decl with
    | SynModuleDecl.Let(_, bindings, _) ->
        printfn "%sLet Declaration" indent
        for binding in bindings do
            visitBinding (depth + 1) binding
            
    | SynModuleDecl.Types(types, _) ->
        printfn "%sType Declaration" indent
        for typ in types do
            printfn "%sType: %s" (String.replicate (depth + 1) "  ") 
                (match typ.Name with SynIdent(ident, _) -> ident.idText)
            
    | _ ->
        printfn "%sOther Declaration: %A" indent decl

// Walk through typed AST (from checkResults)
let visitTypedTree (typedTree: FSharpImplementationFileContents) =
    printfn "\nTyped Tree Walk:"
    for entity in typedTree.Declarations do
        printfn "Entity: %s" entity.DisplayName
        
        // Look at values (let bindings)
        for value in entity.FunctionOrValues do
            printfn "  Value: %s" value.DisplayName
            printfn "    Type: %s" (value.FullType.ToString())

// Example usage
printfn "Untyped AST Walk:"
match parseResults.ParseTree with
| ParsedInput.ImplFile(ParsedImplFileInput(_, _, _, _, _, modules, _, _, _)) ->
    for moduleOrNamespace in modules do
        match moduleOrNamespace with
        | SynModuleOrNamespace(_, _, _, decls, _, _, _, _, _) ->
            for decl in decls do
                visitDecl 1 decl
| _ -> 
    printfn "Could not get parse tree"

// Walk typed tree
match checkResults.ImplementationFile with
| Some typedTree ->
    visitTypedTree typedTree
| None ->
    printfn "Could not get typed tree"