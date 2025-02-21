module TinyFS.Test.WatToWasmTests

open Faqt
open Faqt.Operators
open Helpers
open TinyFS.Core.AstToWasm
open TinyFS.Core.FSharpToAst
open TinyFS.Core.TypeInfos
open Xunit
open FSharp.Compiler.CodeAnalysis

let checker: FSharpChecker = FSharpChecker.Create(keepAssemblyContents = true)

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

let main () = 0
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

let main () = 0
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

let main () = 0
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
let ``Can support single local param`` () =
    let input =
        $"""module Test

let x () =
    let y = 20
    y

let main () = 0
"""

    let declarations = getDeclarations checker input
    let wasmBytes = astToWasm declarations

    //printWasm wasmBytes

    let response = wasmBytes |> runFuncInt32Return "x"
    response.Should().Be(20)

[<Fact>]
let ``Can support local sbyte params`` () =
    let input =
        $"""module Test

let main () =
    let y = 20y
    let z = 23y
    y + z - 3y
"""

    let declarations = getDeclarations checker input
    let wasmBytes = astToWasm declarations

    //printWasm wasmBytes

    let response = wasmBytes |> runFuncInt32Return "main"
    response.Should().Be(40)

[<Fact>]
let ``Can support local int16 params`` () =
    let input =
        $"""module Test

let main () =
    let y = 20s
    let z = 23s
    y + z - 3s
"""

    let declarations = getDeclarations checker input
    let wasmBytes = astToWasm declarations

    //printWasm wasmBytes

    let response = wasmBytes |> runFuncInt32Return "main"
    response.Should().Be(40)

[<Fact>]
let ``Can support local int32 params`` () =
    let input =
        $"""module Test

let x () =
    let y = 20
    let z = 23
    y + z - 3

let main () = 0
"""

    let declarations = getDeclarations checker input
    let wasmBytes = astToWasm declarations

    //printWasm wasmBytes

    let response = wasmBytes |> runFuncInt32Return "x"
    response.Should().Be(40)

[<Fact>]
let ``Can support local int64 params`` () =
    let input =
        $"""module Test

let main () =
    let y = 20L
    let z = 23L
    y + z - 3L
"""

    let declarations = getDeclarations checker input
    let wasmBytes = astToWasm declarations

    printWasm wasmBytes

    let response = wasmBytes |> runFuncInt64Return "main"
    response.Should().Be(40)

[<Fact>]
let ``Can support many int64 params`` () =
    let input =
        $"""module Test

let main () =
    let a = 20L
    let b = 20L
    let c = 20L
    let d = 20L
    let e = 20L
    let f = 20L
    let g = 20L
    let h = 20L
    a + b + c + d + e + f + g + h
"""

    let declarations = getDeclarations checker input
    let wasmBytes = astToWasm declarations

    printWasm wasmBytes

    let response = wasmBytes |> runFuncInt64Return "main"
    response.Should().Be(160)

[<Fact>]
let ``Can support sbyte parameter`` () =
    let input =
        $"""module Test

let add (num1: sbyte) =
    num1 + 3y

let main () = add 3y
"""

    let declarations = getDeclarations checker input
    let wasmBytes = astToWasm declarations

    //printWasm wasmBytes

    let response = wasmBytes |> runFuncInt32Return "main"
    response.Should().Be(6)

[<Fact>]
let ``Can support multiple sbyte parameters`` () =
    let input =
        $"""module Test

let add (num1: sbyte) (num2: sbyte) =
    num1 + 3y + num2

let main () = add 3y 6y
"""

    let declarations = getDeclarations checker input
    let wasmBytes = astToWasm declarations

    //printWasm wasmBytes

    let response = wasmBytes |> runFuncInt32Return "main"
    response.Should().Be(12)

[<Fact>]
let ``Can support int16 parameter`` () =
    let input =
        $"""module Test

let add (num1: int16) =
    num1 + 3s

let main () = add 3s
"""

    let declarations = getDeclarations checker input
    let wasmBytes = astToWasm declarations

    //printWasm wasmBytes

    let response = wasmBytes |> runFuncInt32Return "main"
    response.Should().Be(6)

[<Fact>]
let ``Can support multiple int16 parameters`` () =
    let input =
        $"""module Test

let add (num1: int16) (num2: int16) =
    num1 + 3s + num2

let main () = add 3s 6s
"""

    let declarations = getDeclarations checker input
    let wasmBytes = astToWasm declarations

    //printWasm wasmBytes

    let response = wasmBytes |> runFuncInt32Return "main"
    response.Should().Be(12)

[<Fact>]
let ``Can support int32 parameter`` () =
    let input =
        $"""module Test

let add (num1: int32) =
    num1 + 3

let main () = add 3
"""

    let declarations = getDeclarations checker input
    let wasmBytes = astToWasm declarations

    //printWasm wasmBytes

    let response = wasmBytes |> runFuncInt32Return "main"
    response.Should().Be(6)

[<Fact>]
let ``Can support multiple int32 parameters`` () =
    let input =
        $"""module Test

let add (num1: int32) (num2: int32) =
    num1 + 3 + num2

let main () = add 3 6
"""

    let declarations = getDeclarations checker input
    let wasmBytes = astToWasm declarations

    //printWasm wasmBytes

    let response = wasmBytes |> runFuncInt32Return "main"
    response.Should().Be(12)

[<Fact>]
let ``Can support int64 parameter`` () =
    let input =
        $"""module Test

let add (num1: int64) =
    num1 + 3L

let main () = add 3L
"""

    let declarations = getDeclarations checker input
    let wasmBytes = astToWasm declarations

    //printWasm wasmBytes

    let response = wasmBytes |> runFuncInt64Return "main"
    response.Should().Be(6)

[<Fact>]
let ``Can support multiple int64 parameters`` () =
    let input =
        $"""module Test

let add (num1: int64) (num2: int64) =
    num1 + 3L + num2

let main () = add 3L 6L
"""

    let declarations = getDeclarations checker input
    let wasmBytes = astToWasm declarations

    //printWasm wasmBytes

    let response = wasmBytes |> runFuncInt64Return "main"
    response.Should().Be(12)

[<Theory>]
[<InlineData("y > 0y", 3, 10)>]
[<InlineData("y > 0y", -3, -10)>]
[<InlineData("y < 0y", 3, -10)>]
[<InlineData("y < 0y", -3, 10)>]
[<InlineData("y = 3y", 3, 10)>]
[<InlineData("y = 3y", -3, -10)>]
[<InlineData("y = 3y", 6, -10)>]
[<InlineData("y <> 3y", 3, -10)>]
[<InlineData("y <> 3y", -3, 10)>]
[<InlineData("y <> 3y", 6, 10)>]
[<InlineData("y <= 3y", 3, 10)>]
[<InlineData("y <= 3y", -3, 10)>]
[<InlineData("y <= 3y", 6, -10)>]
[<InlineData("y >= 3y", 3, 10)>]
[<InlineData("y >= 3y", -3, -10)>]
[<InlineData("y >= 3y", 6, 10)>]
let ``Can support sbyte boolean operator expressions`` expr param expected =
    let input =
        $"""module Test

let x (y: sbyte) =
    let z =
        if {expr} then
            10
        else
            -10
    z

let main () = 0
"""

    let declarations = getDeclarations checker input
    let wasmBytes = astToWasm declarations

    //printWasm wasmBytes

    let response = wasmBytes |> runInt32FuncInt32 "x" param
    response.Should().Be(expected)

[<Theory>]
[<InlineData("y > 0s", 3, 10)>]
[<InlineData("y > 0s", -3, -10)>]
[<InlineData("y < 0s", 3, -10)>]
[<InlineData("y < 0s", -3, 10)>]
[<InlineData("y = 3s", 3, 10)>]
[<InlineData("y = 3s", -3, -10)>]
[<InlineData("y = 3s", 6, -10)>]
[<InlineData("y <> 3s", 3, -10)>]
[<InlineData("y <> 3s", -3, 10)>]
[<InlineData("y <> 3s", 6, 10)>]
[<InlineData("y <= 3s", 3, 10)>]
[<InlineData("y <= 3s", -3, 10)>]
[<InlineData("y <= 3s", 6, -10)>]
[<InlineData("y >= 3s", 3, 10)>]
[<InlineData("y >= 3s", -3, -10)>]
[<InlineData("y >= 3s", 6, 10)>]
let ``Can support int16 boolean operator expressions`` expr param expected =
    let input =
        $"""module Test

let x (y: int16) =
    let z =
        if {expr} then
            10
        else
            -10
    z

let main () = 0
"""

    let declarations = getDeclarations checker input
    let wasmBytes = astToWasm declarations

    //printWasm wasmBytes

    let response = wasmBytes |> runInt32FuncInt32 "x" param
    response.Should().Be(expected)

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
let ``Can support int32 boolean operator expressions`` expr param expected =
    let input =
        $"""module Test

let x (y: int32) =
    let z =
        if {expr} then
            10
        else
            -10
    z

let main () = 0
"""

    let declarations = getDeclarations checker input
    let wasmBytes = astToWasm declarations

    //printWasm wasmBytes

    let response = wasmBytes |> runInt32FuncInt32 "x" param
    response.Should().Be(expected)

[<Theory>]
[<InlineData("y > 0L", 3L, 10L)>]
[<InlineData("y > 0L", -3L, -10L)>]
[<InlineData("y < 0L", 3L, -10L)>]
[<InlineData("y < 0L", -3L, 10L)>]
[<InlineData("y = 3L", 3L, 10L)>]
[<InlineData("y = 3L", -3L, -10L)>]
[<InlineData("y = 3L", 6L, -10L)>]
[<InlineData("y <> 3L", 3L, -10L)>]
[<InlineData("y <> 3L", -3L, 10L)>]
[<InlineData("y <> 3L", 6L, 10L)>]
[<InlineData("y <= 3L", 3L, 10L)>]
[<InlineData("y <= 3L", -3L, 10L)>]
[<InlineData("y <= 3L", 6L, -10L)>]
[<InlineData("y >= 3L", 3L, 10L)>]
[<InlineData("y >= 3L", -3L, -10L)>]
[<InlineData("y >= 3L", 6L, 10L)>]
let ``Can support int64 boolean operator expressions`` expr param expected =
    let input =
        $"""module Test

let x (y: int64) =
    let z =
        if {expr} then
            10L
        else
            -10L
    z

let main () = 0
"""

    let declarations = getDeclarations checker input
    let wasmBytes = astToWasm declarations

    //printWasm wasmBytes

    let response = wasmBytes |> runInt64FuncInt64 "x" param
    response.Should().Be(expected)

[<Fact>]
let ``Can throw an error when invalid F# syntax`` () =
    let input =
        $"""module Test

let x (y) =
    let z = y
    z <- z + 3
    z

let main () = 0
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

let main () = 0
"""

    let declarations = getDeclarations checker input
    let wasmBytes = astToWasm declarations

    //printWasm wasmBytes

    let response = wasmBytes |> runInt32FuncInt32 "x" 3
    response.Should().Be(6)

[<Fact>]
let ``Can support while loop`` () =
    let input =
        $"""module Test

let countTo (n) =
    let mutable x = 0
    while x < n do
        x <- x + 1
    x

let main () = 0
"""

    let declarations = getDeclarations checker input
    let wasmBytes = astToWasm declarations

    //printWasm wasmBytes

    let response = wasmBytes |> runInt32FuncInt32 "countTo" 50
    response.Should().Be(50)

[<Fact>]
let ``Can support let parameters in a while loop`` () =
    let input =
        $"""module Test

let countTo (n) =
    let mutable x = 0
    while x < n do
        let y = x
        x <- x + y + 1
    x

let main () = 0
"""

    let declarations = getDeclarations checker input
    let wasmBytes = astToWasm declarations

    //printWasm wasmBytes

    let response = wasmBytes |> runInt32FuncInt32 "countTo" 50
    response.Should().Be(63)

[<Fact>]
let ``Can support boolean expressions`` () =
    let input =
        $"""module Test

let main () =
    let n = 0
    let m = 1
    if n = 0 then
        10
    else
        20
"""

    let declarations = getDeclarations checker input
    let wasmBytes = astToWasm declarations

    //printWasm wasmBytes

    let response = wasmBytes |> runFuncInt32Return "main"
    response.Should().Be(10)

[<Theory>]
[<InlineData("0y", "1y", "n = 0y && m = 1y", 10)>]
[<InlineData("0y", "1y", "n = 1y && m = 1y", 20)>]
[<InlineData("0y", "1y", "n = 0y && m = 0y", 20)>]
[<InlineData("0y", "1y", "n = 1y && m = 0y", 20)>]
[<InlineData("0y", "1y", "n = 0y || m = 1y", 10)>]
[<InlineData("0y", "1y", "n = 1y || m = 1y", 10)>]
[<InlineData("0y", "1y", "n = 0y || m = 0y", 10)>]
[<InlineData("0y", "1y", "n = 1y || m = 0y", 20)>]
let ``Can support sbyte boolean expressions`` nValue mValue expr expected =
    let input =
        $"""module Test

let main () =
    let n: sbyte = {nValue}
    let m: sbyte = {mValue}
    if {expr} then
        10
    else
        20
"""

    let declarations = getDeclarations checker input
    let wasmBytes = astToWasm declarations

    printWasm wasmBytes

    let response = wasmBytes |> runFuncInt32Return "main"
    response.Should().Be(expected)

[<Fact>]
let ``Can support complicated sbyte boolean expression`` () =
    let input =
        $"""module Test

let main () =
    let num1: sbyte = 1y
    let num2: sbyte = 2y
    let num3: sbyte = 3y
    let num4: sbyte = 4y
    let num5: sbyte = 5y
    let num6: sbyte = 6y
    if num1 = 1y && num2 = 2y && num3 = 3y && num4 > 2y && (num5 < 10y || num6 <= 6y) then
        10
    else
        20
"""

    let declarations = getDeclarations checker input
    let wasmBytes = astToWasm declarations

    printWasm wasmBytes

    let response = wasmBytes |> runFuncInt32Return "main"
    response.Should().Be(10)

[<Theory>]
[<InlineData("0s", "1s", "n = 0s && m = 1s", 10)>]
[<InlineData("0s", "1s", "n = 1s && m = 1s", 20)>]
[<InlineData("0s", "1s", "n = 0s && m = 0s", 20)>]
[<InlineData("0s", "1s", "n = 1s && m = 0s", 20)>]
[<InlineData("0s", "1s", "n = 0s || m = 1s", 10)>]
[<InlineData("0s", "1s", "n = 1s || m = 1s", 10)>]
[<InlineData("0s", "1s", "n = 0s || m = 0s", 10)>]
[<InlineData("0s", "1s", "n = 1s || m = 0s", 20)>]
let ``Can support int16 boolean expressions`` nValue mValue expr expected =
    let input =
        $"""module Test

let main () =
    let n: int16 = {nValue}
    let m: int16 = {mValue}
    if {expr} then
        10
    else
        20
"""

    let declarations = getDeclarations checker input
    let wasmBytes = astToWasm declarations

    printWasm wasmBytes

    let response = wasmBytes |> runFuncInt32Return "main"
    response.Should().Be(expected)

[<Theory>]
[<InlineData(0, 1, "n = 0 && m = 1", 10)>]
[<InlineData(0, 1, "n = 1 && m = 1", 20)>]
[<InlineData(0, 1, "n = 0 && m = 0", 20)>]
[<InlineData(0, 1, "n = 1 && m = 0", 20)>]
[<InlineData(0, 1, "n = 0 || m = 1", 10)>]
[<InlineData(0, 1, "n = 1 || m = 1", 10)>]
[<InlineData(0, 1, "n = 0 || m = 0", 10)>]
[<InlineData(0, 1, "n = 1 || m = 0", 20)>]
let ``Can support int32 boolean expressions`` nValue mValue expr expected =
    let input =
        $"""module Test

let main () =
    let n = {nValue}
    let m = {mValue}
    if {expr} then
        10
    else
        20
"""

    let declarations = getDeclarations checker input
    let wasmBytes = astToWasm declarations

    //printWasm wasmBytes

    let response = wasmBytes |> runFuncInt32Return "main"
    response.Should().Be(expected)

[<Theory>]
[<InlineData("0L", "1L", "n = 0L && m = 1L", 10L)>]
[<InlineData("0L", "1L", "n = 1L && m = 1L", 20L)>]
[<InlineData("0L", "1L", "n = 0L && m = 0L", 20L)>]
[<InlineData("0L", "1L", "n = 1L && m = 0L", 20L)>]
[<InlineData("0L", "1L", "n = 0L || m = 1L", 10L)>]
[<InlineData("0L", "1L", "n = 1L || m = 1L", 10L)>]
[<InlineData("0L", "1L", "n = 0L || m = 0L", 10L)>]
[<InlineData("0L", "1L", "n = 1L || m = 0L", 20L)>]
let ``Can support int64 boolean expressions`` nValue mValue expr expected =
    let input =
        $"""module Test

let main () =
    let n: int64 = {nValue}
    let m: int64 = {mValue}
    if {expr} then
        10L
    else
        20L
"""

    let declarations = getDeclarations checker input
    let wasmBytes = astToWasm declarations

    printWasm wasmBytes

    let response = wasmBytes |> runFuncInt64Return "main"
    response.Should().Be(expected)
