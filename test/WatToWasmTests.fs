module Fado.Tests.WatToWasmTests

open TinyFS.Core.FSharpToWat
open TinyFS.Core.WatToWasm
open Faqt
open Helpers
open Xunit

[<Theory>]
[<InlineData("1", 1)>]
//[<InlineData("1 + 3", 4)>]
//[<InlineData("1 - 3", -2)>]
//[<InlineData("10 / 2", 5)>]
//[<InlineData("10 * 15", 150)>]
//[<InlineData("10 * 15 + 10", 160)>]
//[<InlineData("10 * (15 + 10)", 250)>]
let ``Can compile and run simple wasm program`` expr expected =
    let input = $"""
module Test

let x () = {expr}
"""
    let wasmBytes = 
        transformFile input
        |> compile

    printWasm wasmBytes

    let response =
        wasmBytes
        |> runFuncInt32Return "main"
    response.Should().Be(expected)

[<Theory>]
[<InlineData("1", 1)>]
//[<InlineData("1 + 3", 4)>]
//[<InlineData("1 - 3", -2)>]
//[<InlineData("10 / 2", 5)>]
//[<InlineData("10 * 15", 150)>]
//[<InlineData("10 * 15 + 10", 160)>]
//[<InlineData("10 * (15 + 10)", 250)>]
let ``Can generate wasm`` expr expected =
    let input = $"""
module Test

let x = {expr}
"""
    let wasmBytes = 
        transformFile input
        |> generateWasm

    printWasm wasmBytes

    wasmBytes.Should().NotBeEmpty()
    //let response =
    //    wasmBytes
    //    |> runFuncInt32Return "main"
    //response.Should().Be(expected)