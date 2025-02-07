module Fado.Core.WalkThroughTree

open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Compiler.CodeAnalysis

// Create an instance of F# compiler service
let getParseTree () =
    let checker = FSharpChecker.Create()

    // Sample F# code to parse
    //let sourceCode = """
    //module MyModule

    //let add x y = x + y

    //type Person = {
    //    Name: string
    //    Age: int
    //}
    //"""

    let sourceCode = "let x = 2"
    // Create a source text from the code string
    let sourceText = SourceText.ofString sourceCode
    let filename = "script.fsx"
    // Create parsing options
    let (projectOptions, _) = 
        checker.GetProjectOptionsFromScript(
            "script.fsx",
            sourceText,
            assumeDotNetFramework = true)
        |> Async.RunSynchronously
    let opts =
        {
        FSharpParsingOptions.Default with
            SourceFiles = [| filename |]
        }
    // Parse the file
    let parseFileResults = 
        checker.ParseFile(
            filename,
            sourceText,
            opts)
        |> Async.RunSynchronously

    // Get the AST (untyped tree)
    let ast = parseFileResults.ParseTree
    ast
// Function to traverse the AST
let rec visitSyntaxNode depth node =
    let indent = String.replicate depth "  "
    match node with
    | FSharp.Compiler.Syntax.SynModuleDecl.Let(isRecursive, bindings, range) ->
        printfn "%sLet binding at %A" indent range
        for binding in bindings do
            visitBinding (depth + 1) binding
            
    | FSharp.Compiler.Syntax.SynModuleDecl.Types(typeDefinitions, range) ->
        printfn "%sType definition at %A" indent range
        for typeDef in typeDefinitions do
            visitTypeDefinition (depth + 1) typeDef
            
    | _ -> printfn "%sOther node: %A" indent node

and visitBinding depth (binding: FSharp.Compiler.Syntax.SynBinding) =
    let indent = String.replicate depth "  "
    printfn "%sBinding: %A" indent binding.RangeOfBindingWithRhs

and visitTypeDefinition depth (typeDef: FSharp.Compiler.Syntax.SynTypeDefn) =
    let indent = String.replicate depth "  "
    printfn "%sType Definition: %A" indent typeDef.Range

let walk () =
    let ast = getParseTree()
    // Example usage: traverse the AST
    match ast with
    | FSharp.Compiler.Syntax.ParsedInput.ImplFile(ParsedImplFileInput(_, _, _, _, _, modules, _, trivia, identifiers)) ->
        for moduleDecl in modules do
            match moduleDecl with
            | SynModuleOrNamespace (longId, isRecursive, kind, decls, xmlDoc, attribs, accessibility, range, trivia) -> 
                printfn "Module at %A" range
                for decl in decls do
                    visitSyntaxNode 1 decl
    | _ -> printfn "Failed to parse AST"