module TinyFS.Test.TestObjs

open FSharp.Compiler.CodeAnalysis
open System.IO
open FSharp.Compiler.Text
open Fable.Transforms.State
open Fable

//type TestCompiler(currentFile, options, fileText, ?logs: ResizeArray<LogEntry>) =

//    let mutable counter = -1
//    let logs = Option.defaultWith ResizeArray logs

//    member _.fileText = fileText
//    static member Checker: FSharpChecker = FSharpChecker.Create(keepAssemblyContents = true)
//    member _.Declarations =
//        let parseAndCheckSingleFile (checker: FSharpChecker) (input: string) =
//            let file = Path.ChangeExtension(System.IO.Path.GetTempFileName(), ".fsx")
//            File.WriteAllText(file, input)
//            // Get context representing a stand-alone (script) file
//            let projOptions, _errors =
//                checker.GetProjectOptionsFromScript(file, SourceText.ofString input, assumeDotNetFramework = false)
//                |> Async.RunSynchronously

//            checker.ParseAndCheckProject(projOptions)
//            |> Async.RunSynchronously

//        let getDeclarations checker (input: string) =
//            let checkProjectResults = parseAndCheckSingleFile checker input
//            let checkedFile = checkProjectResults.AssemblyContents.ImplementationFiles.[0]

//            checkedFile.Declarations

//        let decls = getDeclarations TestCompiler.Checker fileText
//        decls
//    interface Fable.Compiler with
//        member _.Options = options
//        member _.Plugins: Fable.CompilerPlugins = { MemberDeclarationPlugins = Map.empty }
//        member _.LibraryDir: string = raise (System.NotImplementedException())
//        member _.CurrentFile: string = currentFile
//        member _.OutputDir: string option = raise (System.NotImplementedException())
//        member _.OutputType: Fable.OutputType = raise (System.NotImplementedException())
//        member _.ProjectFile: string = raise (System.NotImplementedException())
//        member _.SourceFiles: string array = raise (System.NotImplementedException())

//        member _.IsPrecompilingInlineFunction: bool = false

//        member this.IncrementCounter() : int =
//            counter <- counter + 1
//            counter

//        member this.WillPrecompileInlineFunction(file: string) : Fable.Compiler =
//            raise (System.NotImplementedException())

//        member this.GetImplementationFile(_: string) =
//            this.Declarations
//            //let parseAndCheckSingleFile (checker: FSharpChecker) (input: string) =
//            //    let file = Path.ChangeExtension(System.IO.Path.GetTempFileName(), ".fsx")
//            //    File.WriteAllText(file, input)
//            //    // Get context representing a stand-alone (script) file
//            //    let projOptions, _errors =
//            //        checker.GetProjectOptionsFromScript(file, SourceText.ofString input, assumeDotNetFramework = false)
//            //        |> Async.RunSynchronously

//            //    checker.ParseAndCheckProject(projOptions)
//            //    |> Async.RunSynchronously

//            //let getDeclarations checker (input: string) =
//            //    let checkProjectResults = parseAndCheckSingleFile checker input
//            //    let checkedFile = checkProjectResults.AssemblyContents.ImplementationFiles.[0]

//            //    checkedFile.Declarations

//            //let decls = getDeclarations TestCompiler.Checker this.fileText
//            //decls

//        member this.GetRootModule(fileName: string) =
//            let fileName = Path.normalizePathAndEnsureFsExtension fileName

//            "", None // failwith msg

//        member this.TryGetEntity(arg1: Fable.AST.Fable.EntityRef) : Fable.AST.Fable.Entity option =
//            raise (System.NotImplementedException())

//        member this.GetInlineExpr(arg1: string) : Fable.InlineExpr =
//            raise (System.NotImplementedException())

//        member this.AddWatchDependency(file: string) : unit =
//            raise (System.NotImplementedException())

//        member _.AddLog(msg, severity, ?range, ?fileName: string, ?tag: string) =
//            LogEntry.Make(severity, msg, ?range = range, ?fileName = fileName, ?tag = tag)
//            |> logs.Add
