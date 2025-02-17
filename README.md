# TinyFS

> Taking inspiration from the [tinygo](https://tinygo.org/) project.

F# for WebAssembly. 

TinyFS is a F# to Wasm Compiler.

Current status: Basic int32 arithmetic operators work. 

Everything is currently run through unit tests, there's no CLI to compile a file or fsproj.

## Installing as Nuget Tool

The fastest way to get started is to install via nuget tool.

* Install a flavor of .NET 9 from microsoft
* Run the following command: `dotnet tool install --global TinyFS.Cli`
  * To update if already installed: `dotnet tool update -g TinyFS.Cli`
* You now have access to the `tinyfs` cli tool

## Usage

How to compile and use.

* Write a valid .fs program containing the supported fs syntax listed below
  * See examples to get started
  * Currently every *.fs file requires main function with a single unit parameter to exist
  * All code must be in a single .fs file, TinyFS does not currently support importing across files.
* Run `tinyfs compile <.fs file>` to generate a wasm file
* Run `tinyfs run <.wasm file>` to run the generated wasm
  * Can also run in any other wasm compliant runtime.
  * This leverages the [wasmtime nuget](https://www.nuget.org/packages/Wasmtime) package.

## How Is This Working

I'm using the [FSharp.Compiler.Service](https://www.nuget.org/packages/FSharp.Compiler.Service) nuget package to generate a usable Abstract Syntax Tree (AST) given F# code.

Then using what I learned in [WebAssembly from the Ground Up](https://wasmgroundup.com/) for converting the F# AST to Wasm.

