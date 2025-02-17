module TinyFS.Test.WatToWasmTests

open Faqt
open Faqt.AssertionHelpers
open Faqt.Operators
open Helpers
open TinyFS.Core.AstToWasm
open TinyFS.Core.FSharpToAst
open TinyFS.Core.FSharpTypes
open Xunit
open FSharp.Compiler.CodeAnalysis

let checker: FSharpChecker = FSharpChecker.Create(keepAssemblyContents = true)

//let getAst declarations = makeCompiler declarations |> generateAst

//let getDeclarations checker (input: string) =
//    let declarations = Util.getDeclarations checker input
//    let fableFile = getAst declarations
//    fableFile.Declarations

let getModuleSymbols (moduleSymbols: ModuleSymbolList) =
    match moduleSymbols.First.Value with
    | Module modd -> modd
    | Function _ -> failwith "TinyFS: Should not be functions in compile"

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

    printWasm wasmBytes

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

    //printWasm wasmBytes

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

    //printWasm wasmBytes

    let response = wasmBytes |> runFuncInt32Return "y"
    response.Should().Be(expected)

[<Fact>]
let ``Can generate module symbols`` () =
    let input =
        $"""module Test

let x () = 1
"""

    let declarations = getDeclarations checker input

    let moduleSymbols =
        buildModuleSymbolList declarations
        |> getModuleSymbols

    % moduleSymbols.Should().HaveLength(1)
    % (moduleSymbols.ContainsKey "x").Should().BeTrue()

    let (xDict, xIndex) = moduleSymbols["x"]

    let paramDict = xDict.paramSymbols
    % paramDict.Count.Should().Be(1)
    % xIndex.Should().Be(0)

    % (paramDict.ContainsKey "unitVar0").Should().BeTrue()
    let symbolEntry = paramDict["unitVar0"]
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
let ``Can generate local params symbols`` () =
    let input =
        $"""module Test

let x () =
    let y = 20
    let z = 23
    y + z
"""

    let declarations = getDeclarations checker input

    let moduleSymbols =
        buildModuleSymbolList declarations
        |> getModuleSymbols

    % moduleSymbols.Should().HaveLength(1)
    % (moduleSymbols.ContainsKey "x").Should().BeTrue()

    let (xDict, xIndex) = moduleSymbols["x"]

    let paramDict = xDict.paramSymbols
    let localDict = xDict.localSymbols
    % paramDict.Count.Should().Be(1)
    % localDict.Count.Should().Be(2)
    % xIndex.Should().Be(0)

    % (paramDict.ContainsKey "unitVar0").Should().BeTrue()
    let xSymbol = paramDict["unitVar0"]
    % xSymbol.isUnit.Should().BeTrue()
    % xSymbol.name.Should().Be("x")

    % (localDict.ContainsKey "y").Should().BeTrue()
    let ySymbol = localDict["y"]
    % ySymbol.isUnit.Should().BeFalse()
    % ySymbol.typ.Should().Be(Types.Int32)
    % ySymbol.name.Should().Be("y")
    % ySymbol.index.Should().Be(0)

    % (localDict.ContainsKey "z").Should().BeTrue()
    let zSymbol = localDict["z"]
    % zSymbol.isUnit.Should().BeFalse()
    % zSymbol.typ.Should().Be(Types.Int32)
    % zSymbol.name.Should().Be("z")
    % zSymbol.index.Should().Be(1)

[<Fact>]
let ``Can handle single local param`` () =
    let input =
        $"""module Test

let x () =
    let y = 20
    y
"""

    let declarations = getDeclarations checker input
    let wasmBytes = astToWasm declarations

    printWasm wasmBytes

    let response = wasmBytes |> runFuncInt32Return "x"
    response.Should().Be(20)

[<Fact>]
let ``Can handle local params`` () =
    let input =
        $"""module Test

let x () =
    let y = 20
    let z = 23
    y + z - 3
"""

    let declarations = getDeclarations checker input
    let wasmBytes = astToWasm declarations

    printWasm wasmBytes

    let response = wasmBytes |> runFuncInt32Return "x"
    response.Should().Be(40)

[<Fact>]
let ``Can handle parameter`` () =
    let input =
        $"""module Test

let x (y) =
    y + 3
"""

    let declarations = getDeclarations checker input
    let wasmBytes = astToWasm declarations

    printWasm wasmBytes

    let response = wasmBytes |> runInt32FuncInt32 "x" 3
    response.Should().Be(6)

[<Theory>]
[<InlineData("y > 0", 3, 10)>]
[<InlineData("y > 0", -3, -10)>]
[<InlineData("y < 0", 3, -10)>]
[<InlineData("y < 0", -3, 10)>]
[<InlineData("y = 3", 3, 10)>]
[<InlineData("y = 3", -3, -10)>]
[<InlineData("y = 3", 6, -10)>]
[<InlineData("y <> 3", 3, -10)>]
[<InlineData("y <> 3", -3, 10)>]
[<InlineData("y <> 3", 6, 10)>]
[<InlineData("y <= 3", 3, 10)>]
[<InlineData("y <= 3", -3, 10)>]
[<InlineData("y <= 3", 6, -10)>]
[<InlineData("y >= 3", 3, 10)>]
[<InlineData("y >= 3", -3, -10)>]
[<InlineData("y >= 3", 6, 10)>]
let ``Can handle boolean operators expressions`` expr param expected =
    let input =
        $"""module Test

let x (y: int32) =
    let z =
        if {expr} then
            10
        else
            -10
    z
"""

    let declarations = getDeclarations checker input
    let wasmBytes = astToWasm declarations

    printWasm wasmBytes

    let response = wasmBytes |> runInt32FuncInt32 "x" param
    response.Should().Be(expected)

[<Fact>]
let ``Can support invalid F# syntax throw an error`` () =
    let input =
        $"""module Test

let x (y) =
    let z = y
    z <- z + 3
    z
"""


    (fun () -> getDeclarations checker input)
        .Should()
        .Throw<System.Exception, _>()

[<Fact>]
let ``Can support mutable variables`` () =
    let input =
        $"""module Test

let x (y) =
    let mutable z = y
    z <- z + 3
    z
"""

    let declarations = getDeclarations checker input
    let wasmBytes = astToWasm declarations

    printWasm wasmBytes

    let response = wasmBytes |> runInt32FuncInt32 "x" 3
    response.Should().Be(6)
