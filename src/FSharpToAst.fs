module TinyFS.Core.FSharpToAst

open FSharp.Compiler.CodeAnalysis
open System.IO
open FSharp.Compiler.Text

let parseAndCheckSingleFile (checker: FSharpChecker) (input: string) =
    let file = Path.ChangeExtension(System.IO.Path.GetTempFileName(), ".fsx")
    File.WriteAllText(file, input)
    // Get context representing a stand-alone (script) file
    let projOptions, _errors =
        checker.GetProjectOptionsFromScript(file, SourceText.ofString input, assumeDotNetFramework = false)
        |> Async.RunSynchronously

    checker.ParseAndCheckProject(projOptions)
    |> Async.RunSynchronously

let sysLib nm =
    let sysDir =
        System.Runtime.InteropServices.RuntimeEnvironment.GetRuntimeDirectory()
    let (++) a b = Path.Combine(a, b)
    sysDir ++ nm + ".dll"

let fsCorePath () =
    "C:\Program Files\dotnet\sdk\9.0.200\FSharp\FSharp.Core.dll"

let parseAndCheckProject (checker: FSharpChecker) (projectPath: string) (projectFilePath: string) =
    // Get project options from the actual F# project file
    let dllName = Path.ChangeExtension(projectFilePath, ".dll")
    let args: string array =
        [| 
           yield "--targetprofile:netcore"
           yield "--target:library"
           yield "--out:MyLibrary.dll"
           yield $"{projectPath}/Library.fs"
           |]
    let projectOptions = 
        checker.GetProjectOptionsFromCommandLineArgs(projectFilePath, args) // projectFilePath args // GetProjectOptionsFromProjectFile(projectFilePath)
        
    // Parse and check the entire project
    let projectResults = 
        checker.ParseAndCheckProject(projectOptions)
        |> Async.RunSynchronously
        
    projectResults

let getDeclarations checker (input: string) =
    let checkProjectResults = parseAndCheckSingleFile checker input
    let checkedFile = checkProjectResults.AssemblyContents.ImplementationFiles.[0]

    let mutable msg = ""

    if (checkProjectResults.Diagnostics.Length > 0) then
        let errorWriter = System.Console.Error

        for diag in checkProjectResults.Diagnostics do
            msg <- msg + "\n" + diag.Message
            errorWriter.WriteLine(diag.Message)

        failwith msg
    else
        checkedFile.Declarations

let getDeclarationsFromProject checker (projectPath: string) (projectFilePath: string) =
    let checkProjectResults = parseAndCheckProject checker projectPath projectFilePath

    // Now you can work with the full project results
    // Example: get all the declarations in the project
    let declarations = 
        checkProjectResults.AssemblyContents.ImplementationFiles
        |> List.map (fun f -> f.Declarations) 
        |> List.collect id

    declarations
    //for file in checkProjectResults.AssemblyContents.ImplementationFiles do
    //    for decl in file.Declarations do
    //        // Process declarations
    //        printfn "  Declaration: %A" decl.
