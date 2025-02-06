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

[<Fact>]
let ``Can walk simple typed AST`` () =
    let result =
        FSharpToWasm.walkInput ("let x = 1")
    let str = WasmAst.printExpression result
    Assert.Equal("x = 1", str)