// For more information see https://aka.ms/fsharp-console-apps
//open Fable
open TinyFS.Shared.Objs
open TinyFS.Core.FSharpToAst
//open TinyFS.Core.FSharpToWat
open TinyFS.Core.AstToWasm
open FSharp.Compiler.CodeAnalysis
open System.IO
open FSharp.Compiler.Text

printfn "Hello from TinyFS"

let input =
    """
module Test

let x (m) = 1 + m
let y () = x(8)
"""


//let checker: FSharpChecker = FSharpChecker.Create(keepAssemblyContents = true)

//let parseAndCheckSingleFile (checker: FSharpChecker) (input: string) =
//    let file = Path.ChangeExtension(System.IO.Path.GetTempFileName(), ".fsx")
//    File.WriteAllText(file, input)
//    // Get context representing a stand-alone (script) file
//    let projOptions, _errors =
//        checker.GetProjectOptionsFromScript(file, SourceText.ofString input, assumeDotNetFramework = false)
//        |> Async.RunSynchronously

//    checker.ParseAndCheckProject(projOptions)
//    |> Async.RunSynchronously

//let getDeclarations checker (input: string) =
//    let checkProjectResults = parseAndCheckSingleFile checker input
//    let checkedFile = checkProjectResults.AssemblyContents.ImplementationFiles.[0]

//    checkedFile.Declarations

//let makeCompiler declarations =
//    //let declarations = getDeclarations checker input
//    let compilerOptions = CompilerOptionsHelper.Make()
//    let compiler = TestCompiler("test", compilerOptions, declarations)
//    printfn "Compiler declarations: %d" compiler.Declarations.Length
//    compiler

//let timeWork() =
//    printfn "\n\nDoing Work"
//    let stopwatch = new System.Diagnostics.Stopwatch()
//    stopwatch.Start()

//    let declarations = getDeclarations checker input
//    printfn "Declaration timing: %d" stopwatch.ElapsedMilliseconds

//    let compiler = makeCompiler declarations

//    printfn "Compiler timing: %d " stopwatch.ElapsedMilliseconds
//    let astResponse = generateAst compiler

//    printfn "AST timing: %d " stopwatch.ElapsedMilliseconds
//    let bytes = compile astResponse.Declarations

//    printfn "Compile timing: %d " stopwatch.ElapsedMilliseconds

//timeWork()
//timeWork()
//timeWork()

//parse()
printfn "Goodbye from TinyFS"
