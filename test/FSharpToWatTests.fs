module Fado.Tests.FSharpToWatTests

open TinyFS.Core
open TinyFS.Core.WatPrinter
open Xunit

[<Fact>]
let ``Can print simple assignment expr`` () =
    let input = """
module Test

let x = 1
"""
    let str = 
        FSharpToWat.transformFile input
        |> printAst
    Assert.Equal("Test_x = 1", str)

[<Theory>]
[<InlineData("1 + 2")>]
[<InlineData("1 - 2")>]
[<InlineData("2 / 1")>]
[<InlineData("2 * 3")>]
[<InlineData("4 % 2")>]
let ``Can print simple arithmetic expr`` expr =
    let input = $"""
module Test

let x = {expr}
"""
    let str = 
        FSharpToWat.transformFile input
        |> printAst
    Assert.Equal($"Test_x = {expr}", str)

[<Fact>]
let ``Can print two expressions`` () =
    let input = """
module Test

let x = 1
x
"""
    let str = 
        FSharpToWat.transformFile input
        |> printAst
    Assert.Equal("Test_x = 1", str)