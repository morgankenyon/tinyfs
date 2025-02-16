module TinyFS.Test.WatToWasmTests

open Faqt
open Faqt.Operators
open Helpers
open TinyFS.Core.AstToWasm
open TinyFS.Core.FSharpToAst
open TinyFS.Shared
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
let ``Can compile and run simple wasm program`` expr expected =
    let input =
        $"""
module Test

let x () = {expr}
"""

    let declarations = getDeclarations checker input
    let wasmBytes = astToWasm declarations

    let response = wasmBytes |> runFuncInt32Return "x"
    response.Should().Be(expected)

//[<Theory>]
//[<InlineData("1", 1)>]
//[<InlineData("1 + 3", 4)>]
//[<InlineData("1 - 3", -2)>]
//[<InlineData("10 / 2", 5)>]
//[<InlineData("11 / 2", 5)>]
//[<InlineData("14 / 5", 2)>]
//[<InlineData("10 * 15", 150)>]
//[<InlineData("10 * 15 + 10", 160)>]
//[<InlineData("10 * (15 + 10)", 250)>]
//let ``Can call other functions`` expr expected =
//    let input =
//        $"""
//module Test

//let x () = {expr}
//let y () = x()
//"""

//    let wasmBytes = getDeclarations checker input |> compile

//    printWasm wasmBytes

//    let response = wasmBytes |> runFuncInt32Return "y"
//    response.Should().Be(expected)

//[<Fact>]
//let ``Can call another function with parameter`` () =
//    let input =
//        $"""
//module Test

//let x (m) = 1 + m
//let y () = x(8)
//"""

//    let wasmBytes = getDeclarations checker input |> compile

//    printWasm wasmBytes

//    let response = wasmBytes |> runFuncInt32Return "y"
//    response.Should().Be(9)

//[<Fact>]
//let ``Can call another function with multiple curried parameters`` () =
//    let input =
//        $"""
//module Test

//let x a b c d = 1 + a + b + c + d
//let y () = x 2 3 4 5
//"""

//    let wasmBytes = getDeclarations checker input |> compile

//    printWasm wasmBytes

//    let response = wasmBytes |> runFuncInt32Return "y"
//    response.Should().Be(15)

//[<Fact>]
//let ``Can call another function with multiple grouped parameters`` () =
//    let input =
//        $"""
//module Test

//let x (a, b, c, d) = 1 + a + b + c + d
//let y () = x(2, 3, 4, 5)
//"""

//    let wasmBytes = getDeclarations checker input |> compile

//    printWasm wasmBytes

//    let response = wasmBytes |> runFuncInt32Return "y"
//    response.Should().Be(15)

//[<Fact>]
//let ``Can test building symbol table with single entry`` () =
//    let input =
//        """
//module Test

//let x = 1
//"""

//    let decls = getDeclarations checker input
//    let functionSymbols = buildModuleSymbolList decls |> getSymbols

//    % functionSymbols.Should().HaveLength(1)
//    let hasKey = functionSymbols.ContainsKey "x"
//    % hasKey.Should().BeTrue()

//[<Fact>]
//let ``Can test building symbol table with three entries`` () =
//    let input =
//        """
//module Test

//let x = 1
//let y = 1
//let z = 1
//"""

//    let decls = getDeclarations checker input
//    let functionSymbols = buildModuleSymbolList decls |> getSymbols

//    % functionSymbols.Should().HaveLength(3)

//    % (functionSymbols.ContainsKey "x").Should().BeTrue()

//    % (functionSymbols.ContainsKey "y").Should().BeTrue()

//    % (functionSymbols.ContainsKey "z").Should().BeTrue()

//    let (xDict, xIndex) = functionSymbols["x"]
//    let (yDict, yIndex) = functionSymbols["y"]
//    let (zDict, zIndex) = functionSymbols["z"]

//    % xDict.Count.Should().Be(0)
//    % xIndex.Should().Be(0)

//    % yDict.Count.Should().Be(0)
//    % yIndex.Should().Be(1)

//    % zDict.Count.Should().Be(0)
//    % zIndex.Should().Be(2)

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
