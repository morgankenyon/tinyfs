module TinyFS.Test.WasmBoolOperatorTests

open Faqt
open Faqt.Operators
open Helpers
open TinyFS.Core.AstToWasm
open TinyFS.Core.FSharpToAst
open TinyFS.Core.TypeInfos
open Xunit
open FSharp.Compiler.CodeAnalysis

let checker = FSharpChecker.Create(keepAssemblyContents = true)

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

[<Theory>]
[<InlineData("y > 0.0", 3.0, 10.0)>]
[<InlineData("y > 0.0", -3.0, -10.0)>]
[<InlineData("y < 0.0", 3.0, -10.0)>]
[<InlineData("y < 0.0", -3.0, 10.0)>]
[<InlineData("y = 3.0", 3.0, 10.0)>]
[<InlineData("y = 3.0", -3.0, -10.0)>]
[<InlineData("y = 3.0", 6.0, -10.0)>]
[<InlineData("y <> 3.0", 3.0, -10.0)>]
[<InlineData("y <> 3.0", -3.0, 10.0)>]
[<InlineData("y <> 3.0", 6.0, 10.0)>]
[<InlineData("y <= 3.0", 3.0, 10.0)>]
[<InlineData("y <= 3.0", -3.0, 10.0)>]
[<InlineData("y <= 3.0", 6.0, -10.0)>]
[<InlineData("y >= 3.0", 3.0, 10.0)>]
[<InlineData("y >= 3.0", -3.0, -10.0)>]
[<InlineData("y >= 3.0", 6.0, 10.0)>]
let ``Can support float64 boolean operator expressions`` expr param expected =
    let input =
        $"""module Test

let x (y: float) =
    let z =
        if {expr} then
            10.0
        else
            -10.0
    z

let main () = 0
"""

    let declarations = getDeclarations checker input
    let wasmBytes = astToWasm declarations

    //printWasm wasmBytes

    let response = wasmBytes |> runFloat64FuncFloat64 "x" param
    response.Should().Be(expected)


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

    //printWasm wasmBytes

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

    //printWasm wasmBytes

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

    //printWasm wasmBytes

    let response = wasmBytes |> runFuncInt32Return "main"
    response.Should().Be(expected)

[<Fact>]
let ``Can support complicated int16 boolean expression`` () =
    let input =
        $"""module Test

let main () =
    let num1 = 1s
    let num2 = 2s
    let num3 = 3s
    let num4 = 4s
    let num5 = 5s
    let num6 = 6s
    if num1 = 1s && num2 = 2s && num3 = 3s && num4 > 2s && (num5 < 10s || num6 <= 6s) then
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

[<Fact>]
let ``Can support complicated int32 boolean expression`` () =
    let input =
        $"""module Test

let main () =
    let num1 = 1
    let num2 = 2
    let num3 = 3
    let num4 = 4
    let num5 = 5
    let num6 = 6
    if num1 = 1 && num2 = 2 && num3 = 3 && num4 > 2 && (num5 < 10 || num6 <= 6) then
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

    //printWasm wasmBytes

    let response = wasmBytes |> runFuncInt64Return "main"
    response.Should().Be(expected)

[<Fact>]
let ``Can support complicated int64 boolean expression`` () =
    let input =
        $"""module Test

let main () =
    let num1 = 1L
    let num2 = 2L
    let num3 = 3L
    let num4 = 4L
    let num5 = 5L
    let num6 = 6L
    if num1 = 1L && num2 = 2L && num3 = 3L && num4 > 2L && (num5 < 10L || num6 <= 6L) then
        10L
    else
        20L
"""

    let declarations = getDeclarations checker input
    let wasmBytes = astToWasm declarations

    //printWasm wasmBytes

    let response = wasmBytes |> runFuncInt64Return "main"
    response.Should().Be(10L)

[<Fact>]
let ``Can support boolean types`` () =
    let input =
        $"""module Test

let main () =
    let bl = true
    if bl then 10 else 0
"""

    let declarations = getDeclarations checker input
    let wasmBytes = astToWasm declarations

    //printWasm wasmBytes

    let response = wasmBytes |> runFuncInt32Return "main"
    response.Should().Be(10)

[<Fact>]
let ``Can support boolean in if statment`` () =
    let input =
        $"""module Test

let main () =
    let bl = true
    if bl && false then 10 else 0
"""

    let declarations = getDeclarations checker input
    let wasmBytes = astToWasm declarations

    //printWasm wasmBytes

    let response = wasmBytes |> runFuncInt32Return "main"
    response.Should().Be(0)

[<Theory>]
[<InlineData("true", "true", "bl1 && bl2", 10)>]
[<InlineData("false", "true", "bl1 && bl2", 0)>]
[<InlineData("true", "false", "bl1 && bl2", 0)>]
[<InlineData("false", "false", "bl1 && bl2", 0)>]
[<InlineData("true", "true", "bl1 || bl2", 10)>]
[<InlineData("false", "true", "bl1 || bl2", 10)>]
[<InlineData("true", "false", "bl1 || bl2", 10)>]
[<InlineData("false", "false", "bl1 || bl2", 0)>]
[<InlineData("true", "true", "(bl1 || bl2) || (bl1 && bl2)", 10)>]
[<InlineData("true", "false", "(bl1 || bl2) || (bl1 && bl2)", 10)>]
[<InlineData("false", "true", "(bl1 || bl2) || (bl1 && bl2)", 10)>]
[<InlineData("false", "false", "(bl1 || bl2) || (bl1 && bl2)", 0)>]
[<InlineData("true", "true", "(bl1 || bl2) && (bl1 && bl2)", 10)>]
[<InlineData("true", "false", "(bl1 || bl2) && (bl1 && bl2)", 0)>]
[<InlineData("false", "true", "(bl1 || bl2) && (bl1 && bl2)", 0)>]
[<InlineData("false", "false", "(bl1 || bl2) && (bl1 && bl2)", 0)>]
let ``Can support complicated bool expressions`` bl1 bl2 expr expected =
    let input =
        $"""module Test

let main () =
    let bl1 = {bl1}
    let bl2 = {bl2}
    if {expr} then 10 else 0
"""

    let declarations = getDeclarations checker input
    let wasmBytes = astToWasm declarations

    //printWasm wasmBytes

    let response = wasmBytes |> runFuncInt32Return "main"
    response.Should().Be(expected)

[<Theory>]
[<InlineData("true", "10", "bl1 && bl2 > 0", 10)>]
[<InlineData("false", "10", "bl1 && bl2 > 0", 0)>]
[<InlineData("true", "-10", "bl1 && bl2 > 0", 0)>]
[<InlineData("false", "-10", "bl1 && bl2 > 0", 0)>]
[<InlineData("true", "10L", "bl1 && bl2 > 0L", 10)>]
[<InlineData("false", "10L", "bl1 && bl2 > 0L", 0)>]
[<InlineData("true", "-10L", "bl1 && bl2 > 0L", 0)>]
[<InlineData("false", "-10L", "bl1 && bl2 > 0L", 0)>]
[<InlineData("true", "10", "bl1 || bl2 > 0", 10)>]
[<InlineData("false", "10", "bl1 || bl2 > 0", 10)>]
[<InlineData("true", "-10", "bl1 || bl2 > 0", 10)>]
[<InlineData("false", "-10", "bl1 || bl2 > 0", 0)>]
[<InlineData("true", "10L", "bl1 || bl2 > 0L", 10)>]
[<InlineData("false", "10L", "bl1 || bl2 > 0L", 10)>]
[<InlineData("true", "-10L", "bl1 || bl2 > 0L", 10)>]
[<InlineData("false", "-10L", "bl1 || bl2 > 0L", 0)>]
[<InlineData("true", "10", "(bl1 || bl2 > 0) || (bl1 && bl2 > 20)", 10)>]
[<InlineData("true", "10", "(bl1 || bl2 > 0) && (bl1 && bl2 > 20)", 0)>]
[<InlineData("true", "10L", "(bl1 || bl2 > 0L) || (bl1 && bl2 > 20L)", 10)>]
[<InlineData("true", "10L", "(bl1 || bl2 > 0L) && (bl1 && bl2 > 20L)", 0)>]
let ``Can support mixed bool expressions`` bl1 bl2 expr expected =
    let input =
        $"""module Test

let main () =
    let bl1 = {bl1}
    let bl2 = {bl2}
    if {expr} then 10 else 0
"""

    let declarations = getDeclarations checker input
    let wasmBytes = astToWasm declarations

    //printWasm wasmBytes

    let response = wasmBytes |> runFuncInt32Return "main"
    response.Should().Be(expected)
