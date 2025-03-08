﻿module TinyFS.Test.AstToWasmTests

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
let ``Can run simple main`` () =
    let input =
        $"""module Test

let main () = 233
"""

    let declarations = getDeclarations checker input
    let wasmBytes = astToWasm declarations

    let response = wasmBytes |> runFuncInt32Return "main"
    response.Should().Be(233)

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

    //printWasm wasmBytes

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

    //printWasm wasmBytes

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
let ``Can support sbyte parameters`` () =
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
let ``Can support int16 parameters`` () =
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
let ``Can support int32 parameters`` () =
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
let ``Can support int64 parameters`` () =
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

[<Fact>]
let ``Can support float64 parameter`` () =
    let input =
        $"""module Test

let add (num1: float) =
    num1 + 3.5

let main () = add 3.0
"""

    let declarations = getDeclarations checker input
    let wasmBytes = astToWasm declarations

    //printWasm wasmBytes

    let response = wasmBytes |> runFuncFloat64Return "main"
    response.Should().Be(6.5)

[<Fact>]
let ``Can support float64 parameters`` () =
    let input =
        $"""module Test

let add (num1: float) (num2: float) =
    num1 + 3.0 + num2

let main () = add 3.25 6.5
"""

    let declarations = getDeclarations checker input
    let wasmBytes = astToWasm declarations

    //printWasm wasmBytes

    let response = wasmBytes |> runFuncFloat64Return "main"
    response.Should().Be(12.75)

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

    printWasm wasmBytes

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
let ``Can support float64 type`` () =
    let input =
        $"""module Test

let main () =
    let num = 3.14
    num
"""

    let declarations = getDeclarations checker input
    let wasmBytes = astToWasm declarations

    //printWasm wasmBytes

    let response = wasmBytes |> runFuncFloat64Return "main"
    response.Should().Be(3.14)
