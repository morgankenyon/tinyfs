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

let parseAndCheckProject (checker: FSharpChecker) (projectFilePath: string) =
    // Get project options from the actual F# project file
    let args: string array = [||]
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

let getDeclarationsFromProject checker (projectFilePath: string) =
    let results = parseAndCheckProject checker projectFilePath

    // Now you can work with the full project results
    // Example: get all the declarations in the project
    for file in results.AssemblyContents.ImplementationFiles do
        printfn "File: %s" file.FileName
        for decl in file.Declarations do
            // Process declarations
            printfn "  Declaration: %A" decl.
