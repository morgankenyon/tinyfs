module Fado.Tests.FSharpToWatTests

open Fado.Core
open Xunit

[<Fact>]
let ``Can walk AST`` () =
    let input = """
module Test

let x = 1
"""
    let response = FSharpToWat.transformFile input
    Assert.Equal(0, response.Length)