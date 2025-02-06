module Fado.Tests.FSharpToWasmTests

open Xunit
open Fado.Core

[<Fact>]
let ``Can walk AST`` () =
    FSharpToWasm.walkTree()
    Assert.True(true)

//[<Fact>]
//let ``Can walk typed AST`` () =
//    FSharpToWasm.walkCheckedTree ()
//    Assert.True(true)

[<Theory>]
[<InlineData("let x = 1", "x = 1")>]
[<InlineData("let x = 1 + 2", "x = 1 + 2")>]
let ``Can walk simple typed AST`` input expected =
    let result = FSharpToWasm.walkInput input
    let str = WasmAst.printExpression result
    Assert.Equal(expected, str)