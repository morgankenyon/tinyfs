module TinyFS.Test.WasmMathOperatorTests

open Faqt
open Faqt.Operators
open FSharp.Compiler.CodeAnalysis
open Helpers
open TinyFS.Core.AstToWasm
open TinyFS.Core.FSharpToAst
open TinyFS.Core.TypeInfos
open Xunit

let checker = FSharpChecker.Create(keepAssemblyContents = true)

[<Theory>]
[<InlineData("1y", 1)>]
[<InlineData("1y + 3y", 4)>]
[<InlineData("1y + 3y + 2y", 6)>]
[<InlineData("1y - 3y", -2)>]
[<InlineData("10y / 2y", 5)>]
[<InlineData("11y / 2y", 5)>]
[<InlineData("14y / 5y", 2)>]
[<InlineData("11y % 2y", 1)>]
[<InlineData("14y % 5y", 4)>]
[<InlineData("10y * 15y", 150)>]
[<InlineData("10y * 15y + 10y", 160)>]
[<InlineData("10y * (15y + 10y)", 250)>]
let ``Can support sbyte expressions`` expr expected =
    let input =
        $"""
module Test

let main () = {expr}
"""

    let declarations = getDeclarations checker input
    let wasmBytes = astToWasm declarations

    //printWasm wasmBytes

    let response = wasmBytes |> runFuncInt32Return "main"
    response.Should().Be(expected)

[<Theory>]
[<InlineData("1s", 1)>]
[<InlineData("1s + 3s", 4)>]
[<InlineData("1s + 3s + 2s", 6)>]
[<InlineData("1s - 3s", -2)>]
[<InlineData("10s / 2s", 5)>]
[<InlineData("11s / 2s", 5)>]
[<InlineData("14s / 5s", 2)>]
[<InlineData("11s % 2s", 1)>]
[<InlineData("14s % 5s", 4)>]
[<InlineData("10s * 15s", 150)>]
[<InlineData("10s * 15s + 10s", 160)>]
[<InlineData("10s * (15s + 10s)", 250)>]
let ``Can support int16 expressions`` expr expected =
    let input =
        $"""
module Test

let main () = {expr}
"""

    let declarations = getDeclarations checker input
    let wasmBytes = astToWasm declarations

    //printWasm wasmBytes

    let response = wasmBytes |> runFuncInt32Return "main"
    response.Should().Be(expected)

[<Theory>]
[<InlineData("1", 1)>]
[<InlineData("1 + 3", 4)>]
[<InlineData("1 + 3 + 2", 6)>]
[<InlineData("1 - 3", -2)>]
[<InlineData("10 / 2", 5)>]
[<InlineData("11 / 2", 5)>]
[<InlineData("14 / 5", 2)>]
[<InlineData("11 % 2", 1)>]
[<InlineData("14 % 5", 4)>]
[<InlineData("10 * 15", 150)>]
[<InlineData("10 * 15 + 10", 160)>]
[<InlineData("10 * (15 + 10)", 250)>]
let ``Can support int32 expressions`` expr expected =
    let input =
        $"""
module Test

let x () = {expr}

let main () = 0
"""

    let declarations = getDeclarations checker input
    let wasmBytes = astToWasm declarations

    //printWasm wasmBytes

    let response = wasmBytes |> runFuncInt32Return "x"
    response.Should().Be(expected)

[<Theory>]
[<InlineData("1L", 1L)>]
[<InlineData("4_294_967_594L", 4_294_967_594L)>]
[<InlineData("1L + 3L", 4L)>]
[<InlineData("2_147_483_657L + 2_147_483_937L", 4_294_967_594L)>]
[<InlineData("1L + 3L + 2L", 6L)>]
[<InlineData("1L - 3L", -2L)>]
[<InlineData("10L / 2L", 5L)>]
[<InlineData("11L / 2L", 5L)>]
[<InlineData("14L / 5L", 2L)>]
[<InlineData("11L % 2L", 1L)>]
[<InlineData("14L % 5L", 4L)>]
[<InlineData("10L * 15L", 150L)>]
[<InlineData("10L * 15L  + 10L", 160L)>]
[<InlineData("10L * (15L + 10L)", 250L)>]
let ``Can support int64 expressions`` expr expected =
    let input =
        $"""
module Test

let main () = {expr}
"""

    let declarations = getDeclarations checker input
    let wasmBytes = astToWasm declarations

    //printWasm wasmBytes

    let response = wasmBytes |> runFuncInt64Return "main"
    response.Should().Be(expected)

[<Theory>]
[<InlineData("1.0f", 1.0f)>]
[<InlineData("1.0f + 3.5f", 4.5f)>]
[<InlineData("1.0f + 3.0f + 2.3f", 6.3f)>]
[<InlineData("1.0f - 3.0f", -2.0f)>]
[<InlineData("10.0f / 2.5f", 4.0f)>]
[<InlineData("11.0f / 2.0f", 5.5f)>]
[<InlineData("14.0f / 5.0f", 2.8f)>]
//[<InlineData("11.0 % 2.0", 1.0)>] //don't know about modulo
//[<InlineData("14.0 % 5.0", 4.0)>] //don't know about modulo
[<InlineData("10.0f * 15.5f", 155.0f)>]
[<InlineData("10.0f * 15.0f + 10.0f", 160.0f)>]
[<InlineData("10.0f * (15.0f + 10.0f)", 250.0f)>]
let ``Can support float32 expressions`` expr expected =
    let input =
        $"""
module Test

let main () = {expr}
"""

    let declarations = getDeclarations checker input
    let wasmBytes = astToWasm declarations

    //printWasm wasmBytes

    let response = wasmBytes |> runFuncFloat32Return "main"
    response.Should().Be(expected)

[<Theory>]
[<InlineData("1.0", 1.0)>]
[<InlineData("1.0 + 3.5", 4.5)>]
[<InlineData("1.0 + 3.0 + 2.3", 6.3)>]
[<InlineData("1.0 - 3.0", -2.0)>]
[<InlineData("10.0 / 2.5", 4.0)>]
[<InlineData("11.0 / 2.0", 5.5)>]
[<InlineData("14.0 / 5.0", 2.8)>]
//[<InlineData("11.0 % 2.0", 1.0)>] //don't know about modulo
//[<InlineData("14.0 % 5.0", 4.0)>] //don't know about modulo
[<InlineData("10.0 * 15.5", 155.0)>]
[<InlineData("10.0 * 15.0 + 10.0", 160.0)>]
[<InlineData("10.0 * (15.0 + 10.0)", 250.0)>]
let ``Can support float64 expressions`` expr expected =
    let input =
        $"""
module Test

let main () = {expr}
"""

    let declarations = getDeclarations checker input
    let wasmBytes = astToWasm declarations

    //printWasm wasmBytes

    let response = wasmBytes |> runFuncFloat64Return "main"
    response.Should().Be(expected)
