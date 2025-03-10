module TinyFS.Test.FSharpToAstTests

open Faqt
open TinyFS.Core.FSharpToAst
open TinyFS.Test.Helpers
open Xunit
open FSharp.Compiler.CodeAnalysis

let checker: FSharpChecker = FSharpChecker.Create(keepAssemblyContents = true)
//[<Fact>]
//let ``Can convert simple let statement to ast`` () =
//    let input =
//        $"""
//module Test

//let x = 1
//"""

//    let declarations = getDeclarations checker input
//    declarations.Should().HaveLength(1)

// [<Fact>]
// let ``Can convert several let statements to ast`` () =
//    let input =
//        $"""
// module Test

// let x = 1
// let y = 1
// let z = 1
// """

//    let declarations = getDeclarations checker input
//    declarations.Should().HaveLength(3)

[<Fact>]
let ``Can compile with underscore in name`` () =
   let input =
       $"""
module Test

let ___mem = []
"""

   let declarations = getDeclarations checker input
   declarations.Should().HaveLength(1)
