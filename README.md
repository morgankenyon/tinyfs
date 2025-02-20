# TinyFS

> Taking inspiration from the [tinygo](https://tinygo.org/) project.

F# for WebAssembly. 

TinyFS is a F# to Wasm Compiler.

Current status: Basic int32 arithmetic operators work. 

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

## Alpha Notice

⚠️⚠️⚠️⚠️⚠️⚠️

This is still alpha software, bugs are to be expected. The more complex the code, the more likely it will break.

## Supported Syntax

I don't think my overal goal is to get to 100%, but probably target the 20% of language features that cover 80% of use cases. The easier a language construct is able to be mapped into WebAssembly the more likely it is to be implemented.

Below lists the following language features that are supported. 

> Please see the [examples](./examples/) folder for examples of compileable F# syntax that is ready to be used.

### Basic Types

> https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/basic-types

- [ ] `bool`
- [ ] `byte`
- [x] `sbyte`
- [x] `int16`
- [ ] `uint16`
- [x] `int/int32`
- [ ] `uint/uint32`
- [ ] `long/int64`
- [ ] `ulong/uint64`
- [ ] `nativeint` - probably never
- [ ] `unativeint` - probably never
- [ ] `decimal`
- [ ] `float32/single` (32 bit)
- [ ] `float/double` (64 bit)
- [ ] `char`
- [ ] `string`
- [ ] `unit`

* The next values to be supported will be:
  * `int64`, `float32`, `float64` and `bool`
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
- [ ] records and untions
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

__Comparison Operators__

- [x] `=`
- [x] `<>`
- [x] `<`
- [x] `<=`
- [x] `>`
- [x] `>=`

__Boolean Operators__
- [ ] `&&`
- [ ] `||`

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

