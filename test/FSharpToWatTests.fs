module Fado.Tests.FSharpToWatTests

open Fado.Core
open Fado.Core.WatPrinter
open Xunit

[<Fact>]
let ``Can extract simple variable declaration`` () =
    let input = """
module Test

let x = 1
"""
    let response = FSharpToWat.transformFile input
    Assert.Equal(1, response.Length)

    let head = response.Head

    match head with
    | WatAst.ModuleDeclaration (md) ->
        Assert.Equal("Test", md.Name)

        Assert.Equal(1, md.Members.Length)

        let firstMember = md.Members.Head

        match firstMember with
        | WatAst.MemberDeclaration (memD) ->
            Assert.Equal("Test_x", memD.Name)

            match memD.Body with
            | WatAst.Value (valueKind) ->
                match valueKind with
                | WatAst.NumberConstant (numValue) ->
                    match numValue with
                    | WatAst.NumberValue.Int32 (num) ->
                        Assert.Equal(1, num)
            | _ -> failwith "Should have been a value"
        | _ -> failwith "Should have been a member"
        //Assert.Equal()
    | _ -> failwith "Should have been a module"

[<Fact>]
let ``Can extract simple variable declaration with addition expr`` () =
    let input = """
module Test

let x = 1 + 2
"""
    let response = FSharpToWat.transformFile input
    Assert.Equal(1, response.Length)

[<Fact>]
let ``Can print simple assignment expr`` () =
    let input = """
module Test

let x = 1
"""
    let str = 
        FSharpToWat.transformFile input
        |> printAst
    Assert.Equal("Test_x = 1", str)
[<Fact>]
let ``Can print simple addition expr`` () =
    let input = """
module Test

let x = 1 + 2
"""
    let str = 
        FSharpToWat.transformFile input
        |> printAst
    Assert.Equal("Test_x = 1 + 2", str)