module TinyFS.Test.WatToWasmTests

open Faqt
open Faqt.Operators
open Helpers
open TinyFS.Core.WatToWasm
open TinyFS.Core.FSharpToAst
open Xunit

//[<Theory>]
//[<InlineData("1", 1)>]
//[<InlineData("1 + 3", 4)>]
//[<InlineData("1 - 3", -2)>]
//[<InlineData("10 / 2", 5)>]
//[<InlineData("10 * 15", 150)>]
//[<InlineData("10 * 15 + 10", 160)>]
//[<InlineData("10 * (15 + 10)", 250)>]
//let ``Can compile and run simple wasm program`` expr expected =
//    let input = $"""
//module Test

//let x = {expr}
//"""
//    let fableFile =
//        makeCompiler input
//        |> generateAst
//    fableFile.Declarations.Should().NotBeEmpty()
    //let wasmBytes = 
    //    transformFile input
    //    |> compile

    //printWasm wasmBytes

    //let response =
    //    wasmBytes
    //    |> runFuncInt32Return "main"
    //response.Should().Be(expected)

//[<Theory>]
//[<InlineData("1", 1)>]
////[<InlineData("1 + 3", 4)>]
////[<InlineData("1 - 3", -2)>]
////[<InlineData("10 / 2", 5)>]
////[<InlineData("10 * 15", 150)>]
////[<InlineData("10 * 15 + 10", 160)>]
////[<InlineData("10 * (15 + 10)", 250)>]
//let ``Can generate wasm`` expr expected =
//    let input = $"""
//module Test

//let x = {expr}
//x
//"""
//    let wasmBytes = 
//        transformFile input
//        |> generateWasm

//    printWasm wasmBytes

//    wasmBytes.Should().NotBeEmpty()
//    //let response =
//    //    wasmBytes
//    //    |> runFuncInt32Return "main"
//    //response.Should().Be(expected)

//[<Fact>]
//let ``Can test building symbol table with single entry`` () =
//    let input = """
//module Test

//let x = 1
//"""
//    let decls = transformFile input
//    let symbolMap = buildSymbolMap decls

//    %symbolMap.Should().HaveLength(1)
//    let hasKey = symbolMap.ContainsKey "Test_x"
//    %hasKey.Should().BeTrue()

//    let xVal = symbolMap["Test_x"]
//    %xVal.index.Should().Be(0)
//    %xVal.symbolType.Should().Be(SymbolType.Local)

//[<Fact>]
//let ``Can test building symbol table with three entries`` () =
//    let input = """
//module Test

//let x = 1
//let y = 1
//let z = 1
//"""
//    let decls = transformFile input
//    let symbolMap = buildSymbolMap decls

//    %symbolMap.Should().HaveLength(3)
//    %(symbolMap.ContainsKey "Test_x").Should().BeTrue()
//    %(symbolMap.ContainsKey "Test_y").Should().BeTrue()
//    %(symbolMap.ContainsKey "Test_z").Should().BeTrue()

//    %(symbolMap["Test_x"]).index.Should().Be(0)
//    %(symbolMap["Test_y"]).index.Should().Be(1)
//    %(symbolMap["Test_z"]).index.Should().Be(2)