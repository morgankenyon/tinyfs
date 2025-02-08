# Fado

A F# to Wasm Compiler

Current status: Nothing works.

Using the FSharpCompilerService to get a F# Abstract Syntax Tree (AST), then converting to a Wasm/Wat AST.

Heavily leveraging [Fable](https://github.com/fable-compiler/Fable/blob/main/src/Fable.Transforms/FSharp2Fable.fs) for converting F# AST to Wasm/Wat AST.