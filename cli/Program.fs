// For more information see https://aka.ms/fsharp-console-apps
open Fable
open TinyFS.Shared.Objs
open TinyFS.Core.FSharpToAst
open TinyFS.Core.AstToWasm

printfn "Hello from Fado"

let input =
    """
module Test

let x (m) = 1 + m
let y () = x(8)
"""

let makeCompiler (input: string) =
    let compilerOptions = CompilerOptionsHelper.Make()
    let compiler = TestCompiler("test", compilerOptions, input)
    compiler

let astResponse = makeCompiler input |> generateAst
let bytes = compile astResponse.Declarations
