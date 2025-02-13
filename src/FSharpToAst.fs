module TinyFS.Core.FSharpToAst

open Fable.Transforms.FSharp2Fable

let generateAst (compiler) =
    Compiler.transformFile compiler