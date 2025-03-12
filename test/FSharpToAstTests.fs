module TinyFS.Test.FSharpToAstTests

open Faqt
open FSharp.Compiler.CodeAnalysis
open System.Reflection
open System
open System.IO
open TinyFS.Core.FSharpToAst
open Xunit

let checker: FSharpChecker = FSharpChecker.Create(keepAssemblyContents = true)
let GetDirectoryPath(assembly: Assembly) =
    let filePath = (new Uri(assembly.Location)).LocalPath;
    Path.GetDirectoryName(filePath);

let joinWithOpSeparator (parts: string list) =
    List.reduce (fun a b -> $"{a}{Path.DirectorySeparatorChar}{b}") parts
[<Fact>]
let ``Can convert simple let statement to ast`` () =
    
    let localDir = GetDirectoryPath(Assembly.GetExecutingAssembly())
    let testProjectPath = 
        [ localDir; "tinyfs_testprojects"; "HelloWorld" ]
        |> joinWithOpSeparator
    let testProjectDir = 
        [ localDir; "tinyfs_testprojects"; "HelloWorld"; "HelloWorld.fsproj" ]
        |> joinWithOpSeparator

    let declarations = getDeclarationsFromProject checker testProjectPath testProjectDir
    declarations.Should().HaveLength(1)

//[<Fact>]
//let ``Can convert several let statements to ast`` () =
//    let input =
//        $"""
//module Test

//let x = 1
//let y = 1
//let z = 1
//"""

//    let declarations = getDeclarations checker input
//    declarations.Should().HaveLength(3)
