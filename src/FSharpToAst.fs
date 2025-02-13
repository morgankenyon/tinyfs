module TinyFS.Core.FSharpToAst

open Fable.Transforms.FSharp2Fable
open Fable.Compiler
open Fable.Compiler.Util
open Fable
open System
open Fable.Transforms.State


//let MakeCompiler(cliArgs, currentFile, project) =
//    let opts = cliArgs.CompilerOptions
//    //printfn "%s" currentFile
//    let fableLibDir = Path.getRelativePath currentFile ""

//    //let watchDependencies =
//    //    if cliArgs.IsWatch then
//    //        Some(HashSet())
//    //    else
//    //        None

//    CompilerImpl(
//        currentFile,
//        project,
//        opts,
//        fableLibDir,
//        OutputType.Library
//    )
let generateAst (compiler) =
    Compiler.transformFile compiler
    //CodeServices.compileFileToFableAST 

//let getCompiler () =
//    async {
//        let projDir = IO.Path.Join(__SOURCE_DIRECTORY__, "../TestProject" ) |> Path.normalizeFullPath
//        let projFile = IO.Path.Join(projDir, "TestProject.fsproj" ) |> Path.normalizeFullPath
//        let sourceFile = IO.Path.Join(projDir, "Program.fs" ) |> Path.normalizeFullPath
//        let cliArgs =
//            let compilerOptions = CompilerOptionsHelper.Make()
//            { CliArgs.ProjectFile = projFile
//              FableLibraryPath = None
//              RootDir = projDir
//              Configuration = "Debug"
//              OutDir = None
//              IsWatch = false
//              Precompile = false
//              PrecompiledLib = None
//              PrintAst = false
//              SourceMaps = false
//              SourceMapsRoot = None
//              NoRestore = false
//              NoCache = false
//              NoParallelTypeCheck = false
//              Exclude = ["Fable.Core"]
//              Replace = Map.empty
//              RunProcess = None
//              CompilerOptions = compilerOptions
//              Verbosity = Verbosity.Normal }

//        let projCracked =
//            ProjectCracked.Init({ cliArgs with NoCache = true }, false, evaluateOnly = true)

//        let checker = InteractiveChecker.Create(projCracked.ProjectOptions)
//        let! assemblies = checker.GetImportedAssemblies()
//        //Fable.Tests.Compiler.Util.
//        let fableProj =
//            Project.From(
//                projCracked.ProjectFile,
//                projCracked.ProjectOptions.SourceFiles,
//                [],
//                assemblies,
//                Fable.Compiler.Util.Log.log
//            )

//        //return FableCompiler(checker, projCracked, fableProj)
//        return projCracked.MakeCompiler("test.fs", fableProj, true)
//    }