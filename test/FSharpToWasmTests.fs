module Fado.Tests.FSharpToWasmTests

open Xunit
open Fado.Core

[<Fact>]
let ``Can walk AST`` () =
    FSharpToWasm.walkTree()
    Assert.True(true)

[<Fact>]
let ``Can walk typed AST`` () =
    FSharpToWasm.walkTypedTree()
    Assert.True(true)