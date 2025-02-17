﻿module TinyFS.Core.FSharpToAst

open FSharp.Compiler.CodeAnalysis
open System.IO
open FSharp.Compiler.Text

//let checker: FSharpChecker = FSharpChecker.Create(keepAssemblyContents = true)

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

    if (checkProjectResults.Diagnostics.Length > 0) then
        let errorWriter = System.Console.Error

        for diag in checkProjectResults.Diagnostics do
            errorWriter.WriteLine(diag.Message)

        failwith ""
    else
        checkedFile.Declarations
