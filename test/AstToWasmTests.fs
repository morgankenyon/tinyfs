﻿module TinyFS.Test.WatToWasmTests

open Faqt
open Faqt.Operators
open Helpers
open TinyFS.Core.AstToWasm
open TinyFS.Core.FSharpToAst
open Xunit
open FSharp.Compiler.CodeAnalysis

let checker: FSharpChecker = FSharpChecker.Create(keepAssemblyContents = true)

//let getAst declarations = makeCompiler declarations |> generateAst

//let getDeclarations checker (input: string) =
//    let declarations = Util.getDeclarations checker input
//    let fableFile = getAst declarations
//    fableFile.Declarations

let getSymbols (moduleSymbols: ModuleSymbolList) =
    match moduleSymbols.First.Value with
    | Function functions -> functions
    | Locals _ -> failwith "TinyFS: Should not be locals in compile"

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
let ``Can compile and run simple wasm expressions`` expr expected =
    let input =
        $"""
module Test

let x () = {expr}
"""

    let declarations = getDeclarations checker input
    let wasmBytes = astToWasm declarations

    let response = wasmBytes |> runFuncInt32Return "x"
    response.Should().Be(expected)

[<Theory>]
[<InlineData("1", 1)>]
[<InlineData("1 + 3", 4)>]
[<InlineData("1 - 3", -2)>]
[<InlineData("10 / 2", 5)>]
[<InlineData("11 / 2", 5)>]
[<InlineData("14 / 5", 2)>]
[<InlineData("10 * 15", 150)>]
[<InlineData("10 * 15 + 10", 160)>]
[<InlineData("10 * (15 + 10)", 250)>]
let ``Can call other functions`` expr expected =
    let input =
        $"""
module Test

let x () = {expr}
let y () = x()
"""

    let declarations = getDeclarations checker input
    let wasmBytes = astToWasm declarations

    printWasm wasmBytes

    let response = wasmBytes |> runFuncInt32Return "y"
    response.Should().Be(expected)

[<Theory>]
[<InlineData("1", 11)>]
[<InlineData("1 + 3", 14)>]
[<InlineData("1 - 3", 8)>]
[<InlineData("10 / 2", 15)>]
[<InlineData("11 / 2", 15)>]
[<InlineData("14 / 5", 12)>]
[<InlineData("10 * 15", 160)>]
[<InlineData("10 * 15 + 10", 170)>]
[<InlineData("10 * (15 + 10)", 260)>]
let ``Can call other functions inside other expressions`` expr expected =
    let input =
        $"""
module Test

let x () = {expr}
let y () = x() + 10
"""

    let declarations = getDeclarations checker input
    let wasmBytes = astToWasm declarations

    printWasm wasmBytes

    let response = wasmBytes |> runFuncInt32Return "y"
    response.Should().Be(expected)

[<Fact>]
let ``Can generated module symbols`` () =
    let input =
        $"""module Test

let x () = 1
"""

    let declarations = getDeclarations checker input
    let funcSymbols = buildModuleSymbolList declarations |> getSymbols

    % funcSymbols.Should().HaveLength(1)
    % (funcSymbols.ContainsKey "x").Should().BeTrue()

    let (xDict, xIndex) = funcSymbols["x"]

    % xDict.Count.Should().Be(1)
    % xIndex.Should().Be(0)

    % (xDict.ContainsKey "unitVar0").Should().BeTrue()
    let symbolEntry = xDict["unitVar0"]
    % symbolEntry.isUnit.Should().BeTrue()
    % symbolEntry.name.Should().Be("x")

[<Fact>]
let ``Can run simple function`` () =
    let input =
        $"""module Test

let x () = 2342
"""

    let declarations = getDeclarations checker input
    let wasmBytes = astToWasm declarations

    let response = wasmBytes |> runFuncInt32Return "x"
    response.Should().Be(2342)

[<Fact>]
let ``Can handle local params`` () =
    let input =
        $"""module Test

let x () =
    let y = 20
    let z = 23
    y + z
"""

    let declarations = getDeclarations checker input
    let wasmBytes = astToWasm declarations

    let response = wasmBytes |> runFuncInt32Return "x"
    response.Should().Be(43)
