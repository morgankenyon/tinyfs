module Fado.Tests.WalkThroughTreeTests

open Fado.Core
open Xunit

[<Fact>]
let ``Can walk AST`` () =
    WalkThroughTree.walk()
    Assert.True(true)