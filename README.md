# TinyFS

[![NuGet version](https://badge.fury.io/nu/TinyFS.Core.svg)](https://badge.fury.io/nu/TinyFS.Core) [![NuGet version](https://badge.fury.io/nu/TinyFS.Cli.svg)](https://badge.fury.io/nu/TinyFS.Cli)

> Taking inspiration from the [tinygo](https://tinygo.org/) project.

F# for WebAssembly. 

TinyFS is a F# to Wasm Compiler.

Leveraging the [FSharp.Compiler.Service](https://www.nuget.org/packages/FSharp.Compiler.Service) nuget package to generate an Abstract Syntax Tree (AST). Then walking the AST to generate Wasm bytes.

Current feature status:
* Support for `int32`, `int64`, `float32`, `float64` and `bool` primitives
* The basic mathematical operators (`+`, `-`, `*`, `/`, `%`) are supported
  * `%` - modulo not supported by floating point primitives
* The basic boolean operators (`=`, `<>`, `>`, `>=`, `<`, `<=`) are supported
* Local parameters are supported, as well as mutable parameters.
* `if/else` expression is supported
  * No `elif` yet
* `while...do`

## Installing as Nuget Tool

The fastest way to get started is to install via nuget tool.

* Install a recent flavor of .NET 9 from microsoft (>9.0.102).
* Run the following command: `dotnet tool install --global TinyFS.Cli`
  * To update if already installed: `dotnet tool update -g TinyFS.Cli`
* You now have access to the `tinyfs` cli tool

## Usage

How to compile and use.

* Write a valid .fs program containing the supported fs syntax listed above/below
  * See examples to get started
  * Currently every *.fs file requires main function with a single unit parameter to exist
  * All code must be in a single .fs file, TinyFS does not currently support importing across files.
* Run `tinyfs compile <.fs file>` to generate a wasm file
* Run `tinyfs run <.wasm file>` to run the generated wasm
  * Can also run in any other wasm compliant runtime.
  * This leverages the [wasmtime nuget](https://www.nuget.org/packages/Wasmtime) package.
* You can also use `tinyfs compile -r <.fs file>` to compile and run in one command.

## Alpha Notice

⚠️⚠️⚠️⚠️⚠️⚠️

This is still alpha software, bugs are to be expected. The more complex the code, the more likely it will break.

By day I mainly program in C#, so I won't claim this library is optimized or error free. I mainly undertook this as a learning opportunity. There are currently no concrete plans for what this project will look like moving forward.

Feel free to raise an issue if you see something could be improved.

## Supported Syntax

Below lists the following language features that are supported. 

The easier a language construct is able to be mapped into WebAssembly the more likely it is to be implemented.

> Please see the [examples](./examples/) folder for examples of compileable F# syntax that is ready to be used.

### Basic Types

> https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/basic-types

- [x] `bool` - by converting to WebAssembly's `i32`
- [ ] `byte`
- [x] `sbyte` - by up converting to WebAssembly's `i32`
- [x] `int16` - by up converting to WebAssembly's `i32`
- [ ] `uint16`
- [x] `int/int32`
- [ ] `uint/uint32`
- [x] `long/int64`
- [ ] `ulong/uint64`
- [ ] `nativeint` - probably never
- [ ] `unativeint` - probably never
- [ ] `decimal`
- [x] `float32/single` (32 bit)
- [x] `float/double` (64 bit)
- [ ] `char`
- [ ] `string`
- [ ] `unit`

* WebAssembly provides builtin support for `int32`, `int64`, `float32` and `float64`. Everything else comes extra.

### Language Constructs

- [x] `let` bindings
- [x] `if...then...else`
- [x] `while...do loops`
- [ ] `for...to loops`
- [ ] `match expression`
- [ ] arrays
- [ ] exception handling
- [ ] generics
- [ ] collections
- [ ] types
- [ ] tuples, options, results
- [ ] records and unions
- [ ] structs
- [ ] object oriented programming (class, interfaces, etc)
- [ ] reflection - probably never
- [ ] computation expressions
- [ ] async/task expressions


### Operators

> https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/symbol-and-operator-reference/

__Arithmetic Operators__
- [x] `+`
- [x] `-`
- [x] `*`
- [x] `/`
- [x] `%`
- [ ] `**`

> For currently supported primitive types

__Comparison Operators__

- [x] `=`
- [x] `<>`
- [x] `<`
- [x] `<=`
- [x] `>`
- [x] `>=`

> For currently supported primitive types

__Boolean Operators__
- [x] `&&`
- [x] `||`

> For currently supported primitive types

__Bitwise Operators__

- [ ] `&&&`
- [ ] `|||`
- [ ] `<<<`
- [ ] `>>>`
- [ ] `^^^`
- [ ] `~~~`

__Function Symbols and Operators__

- [ ] `->`
- [ ] `|>`
- [ ] `||>`
- [ ] `|||>`
- [ ] `>>`
- [ ] `<<`
- [ ] `<|`
- [ ] `<||`
- [ ] `<|||`

### Other Features

- [ ] Importing code across files
- [ ] Building an entire *.fsproj file
- [ ] Using standard library

## How Is This Working

I'm using the [FSharp.Compiler.Service](https://www.nuget.org/packages/FSharp.Compiler.Service) nuget package to generate a usable Abstract Syntax Tree (AST) given F# code.

Then using what I learned in [WebAssembly from the Ground Up](https://wasmgroundup.com/) for converting the F# AST to Wasm.

