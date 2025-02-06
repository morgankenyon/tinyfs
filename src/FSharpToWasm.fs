module Fado.Core.FSharpToWasm

open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Compiler.CodeAnalysis


let get =
    function
    | Ok x -> x
    | Error e -> failwithf "%A" e

let rec getSynConst (syn: SynConst) : byte array =
    match syn with
    | SynConst.Int32 (i32) -> 
        i32
        [||]
    | _ -> 
        [||]

//https://github.com/dotnet/fsharp/blob/main/src/Compiler/SyntaxTree/SyntaxTree.fsi#L563
let rec getSynExpr (syn: SynExpr) : byte array =
    match syn with
    | SynExpr.App (_, isInfix, funcExpr, argExpr, _) -> 
        let funcBytes = getSynExpr funcExpr
        let argBytes = getSynExpr argExpr
        Array.concat [ funcBytes; argBytes ]
    | SynExpr.Const (synConst, _) ->
        
        getSynConst synConst
    | _ -> 
        [||]

let rec getSynModuleDecl (syn: SynModuleDecl) =
    let emptyBytes: byte array = [||]
    match syn with
    | SynModuleDecl.Let (_, bindings, _) -> 
        let firstBinding = bindings |> List.head

        match firstBinding with
        //https://github.com/dotnet/fsharp/blob/main/src/Compiler/SyntaxTree/SyntaxTree.fsi#L1230
        | SynBinding (_, _, _, _, _, _, _, _, _, expr, _, _, _) ->
            match expr with
            | SynExpr.App (_, isInfix, funcExpr, argExpr, _) ->
                let argExprBytes = getSynExpr argExpr
                let funcExprBytes = getSynExpr funcExpr
                funcExprBytes
            | _ -> emptyBytes
    | SynModuleDecl.Expr (e, _) -> 
        e
        emptyBytes
    | _ -> 
        emptyBytes
    
//https://github.com/dotnet/fsharp/blob/main/src/Compiler/SyntaxTree/SyntaxTree.fsi#L1743-1743
let getSynModule (syn: SynModuleOrNamespace) =
    match syn with
    | SynModuleOrNamespace (longId, isRecursive, kind, decls, xmlDoc, attribs, accessibility, range, trivia) -> 
        let firstDecl = decls |> List.head
        //let bindings = firstDecl.
        let moduleBytes = getSynModuleDecl firstDecl
        syn


let tryParseExpression (input : string) =
  async {
    let checker = FSharpChecker.Create()

    let filename = "snippet.fsx"

    let sourceText = SourceText.ofString input

    let opts =
      {
        FSharpParsingOptions.Default with
          SourceFiles = [| filename |]
      }

    
    let! parseResults = checker.ParseFile(filename, sourceText, opts)

    if parseResults.ParseHadErrors then
      return Error parseResults.Diagnostics
    else
      match parseResults.ParseTree with
      | ParsedInput.ImplFile (ParsedImplFileInput (fn, true, qnf, scopedPragmas, hashDirectives, [ m ], flags, trivia, identifiers)) when fn = filename ->
        return Ok m
      | _ ->
        return failwithf "Unexpected parse tree: %A" parseResults.ParseTree
  }
let getUntypedTree (file, input) = 
    let checker = FSharpChecker.Create()
    // Get compiler options for the 'project' implied by a single script file
    let projOptions, diagnostics = 
        checker.GetProjectOptionsFromScript(file, input, assumeDotNetFramework=false)
        |> Async.RunSynchronously

    let parsingOptions, _errors = checker.GetParsingOptionsFromProjectOptions(projOptions)

    // Run the first phase (untyped parsing) of the compiler
    let parseFileResults = 
        checker.ParseFile(file, input, parsingOptions) 
        |> Async.RunSynchronously

    parseFileResults.ParseTree

let getTypedTree (file, input) =
    let checker = FSharpChecker.Create()
    // Get compiler options for the 'project' implied by a single script file
    let projOptions, diagnostics = 
        checker.GetProjectOptionsFromScript(file, input, assumeDotNetFramework=false)
        |> Async.RunSynchronously

    let parsingOptions, _errors = checker.GetParsingOptionsFromProjectOptions(projOptions)

    // Run the first phase (untyped parsing) of the compiler
    let parseFileResults = 
        checker.ParseFile(file, input, parsingOptions) 
        |> Async.RunSynchronously

    parseFileResults.ParseTree
    

let rec visitPattern = function
    | SynPat.Wild _ -> 
        printfn "  .. underscore pattern"
    | SynPat.Named(ident = SynIdent(ident = name)) ->
        printfn "  .. named as '%s'" name.idText
    | SynPat.LongIdent(longDotId = SynLongIdent(id = ident)) ->
        let names = String.concat "." [ for i in ident -> i.idText ]
        printfn "  .. identifier: %s" names
    | pat -> printfn "  .. other pattern: %A" pat

let rec visitExpression e = 
    match e with
    | SynExpr.IfThenElse(ifExpr=cond; thenExpr=trueBranch; elseExpr=falseBranchOpt) ->
        // Visit all sub-expressions
        printfn "Conditional:"
        visitExpression cond
        visitExpression trueBranch
        falseBranchOpt |> Option.iter visitExpression 

    | SynExpr.LetOrUse(_, _, bindings, body, _, _) ->
        // Visit bindings (there may be multiple 
        // for 'let .. = .. and .. = .. in ...'
        printfn "LetOrUse with the following bindings:"
        for binding in bindings do
        let (SynBinding(headPat = headPat; expr = init)) = binding
        visitPattern headPat
        visitExpression init
        // Visit the body expression
        printfn "And the following body:"
        visitExpression body
    | expr -> printfn " - not supported expression: %A" expr

/// Walk over a list of declarations in a module. This is anything
/// that you can write as a top-level inside module (let bindings,
/// nested modules, type declarations etc.)
let visitDeclarations decls = 
    for declaration in decls do
        match declaration with
        | SynModuleDecl.Let(isRec, bindings, range) ->
            // Let binding as a declaration is similar to let binding
            // as an expression (in visitExpression), but has no body
            for binding in bindings do
                let (SynBinding(headPat = pat; expr = body)) = binding
                visitPattern pat 
                visitExpression body         
        | _ -> printfn " - not supported declaration: %A" declaration
/// Walk over all module or namespace declarations 
/// (basically 'module Foo =' or 'namespace Foo.Bar')
/// Note that there is one implicitly, even if the file
/// does not explicitly define it..
let visitModulesAndNamespaces modulesOrNss =
    for moduleOrNs in modulesOrNss do
        let (SynModuleOrNamespace(longId = lid; decls = decls)) = moduleOrNs
        printfn "Namespace or module: %A" lid
        visitDeclarations decls

let walkTree() =
    let input =
      """
      let foo() = 
        let msg = "Hello world"
        if true then 
          printfn "%s" msg
      """

    // File name in Unix format
    let file = "/home/user/Test.fsx"

    // Get the AST of sample F# code
    let tree = getUntypedTree(file, SourceText.ofString input)
    // Extract implementation file details
    match tree with
    | ParsedInput.ImplFile(implFile) ->
        // Extract declarations and walk over them
        let (ParsedImplFileInput(contents = modules)) = implFile
        visitModulesAndNamespaces modules
    | _ -> failwith "F# Interface file (*.fsi) not supported."

let walkTypedTree () =
    let checker = FSharpChecker.Create()
    // Sample input as a multi-line string
    let input =
        """
      open System

      let foo() =
        let msg = String.Concat("Hello"," ","world")
        if true then
          printfn "%s" msg.
      """
    // Split the input & define file name
    let inputLines = input.Split('\n')
    let file = "/home/user/Test.fsx"

    let projOptions, _diagnostics =
        checker.GetProjectOptionsFromScript(file, SourceText.ofString input, assumeDotNetFramework=false)
        |> Async.RunSynchronously

    let parsingOptions, _diagnostics2 =
        checker.GetParsingOptionsFromProjectOptions(projOptions)

    let parseResults, checkFileAnswer =
        checker.ParseAndCheckFileInProject(file, 0, SourceText.ofString input, projOptions)
        |> Async.RunSynchronously

    let checkFileResults =
        match checkFileAnswer with
        | FSharpCheckFileAnswer.Succeeded (res) -> res
        | res -> failwithf "Parsing did not finish... (%A)" res
    ()
    //checkFileResults.GetToolTip()