module TinyFS.Test.FSharpToAstTests

open Faqt
open TinyFS.Core.FSharpToAst
open TinyFS.Test.Helpers
open Xunit

[<Fact>]
let ``Can convert simple let statement to ast`` () =
    let input = $"""
module Test

let x = 1
"""
    let fableFile =
        makeCompiler input
        |> generateAst
    fableFile.Declarations.Should().HaveLength(1)

[<Fact>]
let ``Can convert several let statements to ast`` () =
    let input = $"""
module Test

let x = 1
let y = 1
let z = 1
"""
    let fableFile =
        makeCompiler input
        |> generateAst
    fableFile.Declarations.Should().HaveLength(3)