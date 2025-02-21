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
