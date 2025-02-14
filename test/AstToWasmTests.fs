module TinyFS.Test.WatToWasmTests

open Faqt
open Faqt.Operators
open Helpers
open TinyFS.Core.AstToWasm
open TinyFS.Core.FSharpToAst
open Xunit

let getAst (input: string) = makeCompiler input |> generateAst

let getDeclarations (input: string) =
    let fableFile = getAst input
    fableFile.Declarations

let getSymbols (symbolScope: SymbolScope) =
    match symbolScope.First.Value with
    | Nested nested -> nested
    | Locals _ -> failwith "TinyFS: Should not be locals in compile"

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
let ``Can compile and run simple wasm program`` expr expected =
    let input =
        $"""
module Test

let x = {expr}
"""

    let wasmBytes = getDeclarations input |> compile

    printWasm wasmBytes

    let response = wasmBytes |> runFuncInt32Return "Test_x"
    response.Should().Be(expected)

[<Fact>]
let ``Can test building symbol table with single entry`` () =
    let input =
        """
module Test

let x = 1
"""

    let decls = getDeclarations input
    let symbols = buildSymbolMap decls |> getSymbols

    % symbols.Should().HaveLength(1)
    let hasKey = symbols.ContainsKey "Test_x"
    % hasKey.Should().BeTrue()

[<Fact>]
let ``Can test building symbol table with three entries`` () =
    let input =
        """
module Test

let x = 1
let y = 1
let z = 1
"""

    let decls = getDeclarations input
    let symbols = buildSymbolMap decls |> getSymbols

    % symbols.Should().HaveLength(3)
    % (symbols.ContainsKey "Test_x").Should().BeTrue()
    % (symbols.ContainsKey "Test_y").Should().BeTrue()
    % (symbols.ContainsKey "Test_z").Should().BeTrue()

    let (xDict, xIndex) = symbols["Test_x"]
    let (yDict, yIndex) = symbols["Test_y"]
    let (zDict, zIndex) = symbols["Test_z"]

    % xDict.Count.Should().Be(0)
    % xIndex.Should().Be(0)

    % yDict.Count.Should().Be(0)
    % yIndex.Should().Be(1)

    % zDict.Count.Should().Be(0)
    % zIndex.Should().Be(2)

//[<Fact>]
//let ``Can build function declarations`` () =
