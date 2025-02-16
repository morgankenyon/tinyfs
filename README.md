# TinyFS

> Taking inspiration from the [tinygo](https://tinygo.org/) project.

F# for WebAssembly. 

TinyFS is a F# to Wasm Compiler.

Current status: Basic int32 arithmetic operators work. 

Everything is currently run through unit tests, there's no CLI to compile a file or fsproj.

## How Is This Working

I'm using the [FSharp.Compiler.Service](https://www.nuget.org/packages/FSharp.Compiler.Service) nuget package to generate a usable Abstract Syntax Tree (AST) given F# code.

Then using what I learned in [WebAssembly from the Ground Up](https://wasmgroundup.com/) for converting the F# AST to Wasm.

