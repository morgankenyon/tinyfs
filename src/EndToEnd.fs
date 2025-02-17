module TinyFS.Core.EndToEnd

open FSharp.Compiler.CodeAnalysis

let checker: FSharpChecker = FSharpChecker.Create(keepAssemblyContents = true)

let compile (input: string) : byte list =
    FSharpToAst.getDeclarations checker input
    |> AstToWasm.astToWasm
