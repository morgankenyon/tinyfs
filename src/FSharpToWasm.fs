module Fado.Core.FSharpToWasm

open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Compiler.CodeAnalysis

open FSharp.Compiler.EditorServices


open FSharp.Compiler.Symbols


open System
open System.IO
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.EditorServices
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text

open WasmAst


let checker = FSharpChecker.Create(keepAssemblyContents=true)


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

let rec visitExpr f (e:FSharpExpr): Expression list = 
    f e
    match e with 
    //| FSharpExprPatterns.AddressOf(lvalueExpr) -> 
    //    visitExpr f lvalueExpr
    //| FSharpExprPatterns.AddressSet(lvalueExpr, rvalueExpr) -> 
    //    visitExpr f lvalueExpr; visitExpr f rvalueExpr
    //| FSharpExprPatterns.Application(funcExpr, typeArgs, argExprs) -> 
    //    visitExpr f funcExpr; visitExprs f argExprs
    | FSharpExprPatterns.Call(objExprOpt, memberOrFunc, typeArgs1, typeArgs2, argExprs) -> 
        visitObjArg f objExprOpt; visitExprs f argExprs
    //| FSharpExprPatterns.Coerce(targetType, inpExpr) -> 
    //    visitExpr f inpExpr
    //| FSharpExprPatterns.FastIntegerForLoop(startExpr, limitExpr, consumeExpr, isUp, _, _) -> 
    //    visitExpr f startExpr; visitExpr f limitExpr; visitExpr f consumeExpr
    //| FSharpExprPatterns.ILAsm(asmCode, typeArgs, argExprs) -> 
    //    visitExprs f argExprs
    //| FSharpExprPatterns.ILFieldGet (objExprOpt, fieldType, fieldName) -> 
    //    visitObjArg f objExprOpt
    //| FSharpExprPatterns.ILFieldSet (objExprOpt, fieldType, fieldName, valueExpr) -> 
    //    visitObjArg f objExprOpt
    //| FSharpExprPatterns.IfThenElse (guardExpr, thenExpr, elseExpr) -> 
    //    visitExpr f guardExpr; visitExpr f thenExpr; visitExpr f elseExpr
    //| FSharpExprPatterns.Lambda(lambdaVar, bodyExpr) -> 
    //    visitExpr f bodyExpr
    //| FSharpExprPatterns.Let((bindingVar, bindingExpr, dbg), bodyExpr) -> 
    //    visitExpr f bindingExpr; visitExpr f bodyExpr
    //| FSharpExprPatterns.LetRec(recursiveBindings, bodyExpr) ->
    //    for _,bindingExpr,_ in recursiveBindings do visitExpr f bindingExpr
    //    visitExpr f bodyExpr
    //| FSharpExprPatterns.NewArray(arrayType, argExprs) -> 
    //    visitExprs f argExprs
    //| FSharpExprPatterns.NewDelegate(delegateType, delegateBodyExpr) -> 
    //    visitExpr f delegateBodyExpr
    //| FSharpExprPatterns.NewObject(objType, typeArgs, argExprs) -> 
    //    visitExprs f argExprs
    //| FSharpExprPatterns.NewRecord(recordType, argExprs) ->  
    //    visitExprs f argExprs
    //| FSharpExprPatterns.NewAnonRecord(recordType, argExprs) ->  
    //    visitExprs f argExprs
    //| FSharpExprPatterns.NewTuple(tupleType, argExprs) -> 
    //    visitExprs f argExprs
    //| FSharpExprPatterns.NewUnionCase(unionType, unionCase, argExprs) -> 
    //    visitExprs f argExprs
    //| FSharpExprPatterns.Quote(quotedExpr) -> 
    //    visitExpr f quotedExpr
    //| FSharpExprPatterns.FSharpFieldGet(objExprOpt, recordOrClassType, fieldInfo) -> 
    //    visitObjArg f objExprOpt
    //| FSharpExprPatterns.AnonRecordGet(objExpr, recordOrClassType, fieldInfo) -> 
    //    visitExpr f objExpr
    //| FSharpExprPatterns.FSharpFieldSet(objExprOpt, recordOrClassType, fieldInfo, argExpr) -> 
    //    visitObjArg f objExprOpt; visitExpr f argExpr
    //| FSharpExprPatterns.Sequential(firstExpr, secondExpr) -> 
    //    visitExpr f firstExpr; visitExpr f secondExpr
    //| FSharpExprPatterns.TryFinally(bodyExpr, finalizeExpr, dbgTry, dbgFinally) -> 
    //    visitExpr f bodyExpr; visitExpr f finalizeExpr
    //| FSharpExprPatterns.TryWith(bodyExpr, _, _, catchVar, catchExpr, dbgTry, dbgWith) -> 
    //    visitExpr f bodyExpr; visitExpr f catchExpr
    //| FSharpExprPatterns.TupleGet(tupleType, tupleElemIndex, tupleExpr) -> 
    //    visitExpr f tupleExpr
    //| FSharpExprPatterns.DecisionTree(decisionExpr, decisionTargets) -> 
    //    visitExpr f decisionExpr; List.iter (snd >> visitExpr f) decisionTargets
    //| FSharpExprPatterns.DecisionTreeSuccess (decisionTargetIdx, decisionTargetExprs) -> 
    //    visitExprs f decisionTargetExprs
    //| FSharpExprPatterns.TypeLambda(genericParam, bodyExpr) -> 
    //    visitExpr f bodyExpr
    //| FSharpExprPatterns.TypeTest(ty, inpExpr) -> 
    //    visitExpr f inpExpr
    //| FSharpExprPatterns.UnionCaseSet(unionExpr, unionType, unionCase, unionCaseField, valueExpr) -> 
    //    visitExpr f unionExpr; visitExpr f valueExpr
    //| FSharpExprPatterns.UnionCaseGet(unionExpr, unionType, unionCase, unionCaseField) -> 
    //    visitExpr f unionExpr
    //| FSharpExprPatterns.UnionCaseTest(unionExpr, unionType, unionCase) -> 
    //    visitExpr f unionExpr
    //| FSharpExprPatterns.UnionCaseTag(unionExpr, unionType) -> 
    //    visitExpr f unionExpr
    //| FSharpExprPatterns.ObjectExpr(objType, baseCallExpr, overrides, interfaceImplementations) -> 
    //    visitExpr f baseCallExpr
    //    List.iter (visitObjMember f) overrides
    //    List.iter (snd >> List.iter (visitObjMember f)) interfaceImplementations
    //| FSharpExprPatterns.TraitCall(sourceTypes, traitName, typeArgs, typeInstantiation, argTypes, argExprs) -> 
    //    visitExprs f argExprs
    //| FSharpExprPatterns.ValueSet(valToSet, valueExpr) -> 
    //    visitExpr f valueExpr
    //| FSharpExprPatterns.WhileLoop(guardExpr, bodyExpr, dbg) -> 
    //    visitExpr f guardExpr; visitExpr f bodyExpr
    //| FSharpExprPatterns.BaseValue baseType -> ()
    //| FSharpExprPatterns.DefaultValue defaultType -> ()
    //| FSharpExprPatterns.ThisValue thisType -> ()
    | FSharpExprPatterns.Const(constValueObj, constType) ->
        //Literal 
        //match constType.AbbreviatedType with
        //| System.Int32
        //()
        //if constType.Equals(typeof<int32>) then
        //else 
        let ttt = constType.GetType()
        match constType.TypeDefinition.LogicalName with
        | "int32" | "int" ->
            let lit = 
                Int32.Parse(constValueObj.ToString())
                |> IntLiteral
                
            let iden = 
                {
                    
                }
            [(IdentExpr lit)]
            //let ident = { Name = }
        | _ -> failwith((sprintf "unsupported type: %s" constType.BasicQualifiedName))
    //| FSharpExprPatterns.Value(valueToGet) -> 
    //    23 |> IntLiteral
        //()
    | _ -> failwith (sprintf "unrecognized %+A" e)
and visitExprs f exprs = 
    let exprs = 
        List.map (visitExpr f) exprs
        |> List.collect id
    exprs

and visitObjArg f objOpt : Expression list = 
    let result =
        objOpt
        |> Option.map (visitExpr f)
    match result with
    | Some n -> n
    | _ -> []
//and visitObjMember f memb = 
//    visitExpr f memb.Body

let buildIdentifierExpression f (v: FSharpMemberOrFunctionOrValue) (e: FSharpExpr) =
    //let vall = visitExpr f v
    let name = v.DisplayName
    let lit = visitExpr f e
    let ident = 
        {
            Name = name
            Literal = lit
        }
    ident
    
let rec visitFileDecl f (mem: FSharpImplementationFileDeclaration) = 
    match mem with 
        | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(v, vs, e) -> 
            if vs.Length = 0 
            then
                let result = buildIdentifierExpression f v e
                IdentExpr result
            else failwith "functions are unsupported right now"
        | _ -> failwith "unexpected"
    //FSharpImplementationFileDeclaration.MemberOrFunctionOrValue

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

let generateFileResults () =
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
    checkFileResults
    //checkFileResults.GetToolTip()

let parseAndCheckSingleFile (input: string) = 
    let file = Path.ChangeExtension(System.IO.Path.GetTempFileName(), "fsx")  
    File.WriteAllText(file, input)
    // Get context representing a stand-alone (script) file
    let projOptions, _errors = 
        checker.GetProjectOptionsFromScript(file, SourceText.ofString input, assumeDotNetFramework=false)
        |> Async.RunSynchronously

    checker.ParseAndCheckProject(projOptions) 
    |> Async.RunSynchronously

let rec printDecl prefix d = 
    match d with 
    | FSharpImplementationFileDeclaration.Entity (e, subDecls) -> 
        printfn "%sEntity %s was declared and contains %d sub-declarations" prefix e.CompiledName subDecls.Length
        for subDecl in subDecls do 
            printDecl (prefix+"    ") subDecl
    | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(v, vs, e) -> 
        printfn "%sMember or value %s was declared" prefix  v.CompiledName
    | FSharpImplementationFileDeclaration.InitAction(e) -> 
        printfn "%sA top-level expression was declared" prefix 
//let walkCheckedTree () =
//    let input =
//        """
//      open System

//      let foo() =
//        let msg = String.Concat("Hello"," ","world")
//        if true then
//          printfn "%s" msg.
//      """
//    let checkProjectResults = parseAndCheckSingleFile input
//    let checkedFile = checkProjectResults.AssemblyContents.ImplementationFiles.[0]

//    for d in checkedFile.Declarations do 
//       printDecl "" d
//    let myLibraryEntity, myLibraryDecls =    
//       match checkedFile.Declarations.[0] with 
//       | FSharpImplementationFileDeclaration.Entity (e, subDecls) -> (e, subDecls)
//       | _ -> failwith "unexpected"

//    let (fooSymbol, fooArgs, fooExpression) = 
//        match myLibraryDecls.[0] with 
//        | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(v, vs, e) -> (v, vs, e)
//        | _ -> failwith "unexpected"
//    fooExpression |> visitExpr (fun e -> printfn "Visiting %A" e)

let walkInput (input: string) =    
    let checkProjectResults = parseAndCheckSingleFile input
    let checkedFile = checkProjectResults.AssemblyContents.ImplementationFiles.[0]

    for d in checkedFile.Declarations do 
       printDecl "" d
    let myLibraryEntity, myLibraryDecls =    
       match checkedFile.Declarations.[0] with 
       | FSharpImplementationFileDeclaration.Entity (e, subDecls) -> (e, subDecls)
       | _ -> failwith "unexpected"

    let result =
        myLibraryDecls.[0]
        |> visitFileDecl (fun e -> printfn "Visiting %A" e)
    //let (fooSymbol, fooArgs, fooExpression) = 
    //    match myLibraryDecls.[0] with 
    //    | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(v, vs, e) -> (v, vs, e)
    //    | _ -> failwith "unexpected"
    //let result =
    //    fooExpression
    //    |> visitExpr (fun e -> printfn "Visiting %A" e)
    result