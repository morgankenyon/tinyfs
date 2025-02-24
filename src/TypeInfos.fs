module TinyFS.Core.TypeInfos

open FSharp.Compiler.Symbols
open System.Collections.Generic

[<Literal>]
let FS_SBYTE = "Microsoft.FSharp.Core.sbyte"

[<Literal>]
let FS_INT16 = "Microsoft.FSharp.Core.int16"

[<Literal>]
let FS_INT64 = "Microsoft.FSharp.Core.int64"

[<Literal>]
let FS_UNIT = "Microsoft.FSharp.Core.unit"

[<Literal>]
let FS_INT32 = "Microsoft.FSharp.Core.int32"

[<Literal>]
let FS_INT = "Microsoft.FSharp.Core.int"

[<Literal>]
let FS_BOOL = "Microsoft.FSharp.Core.bool"

[<Literal>]
let FS_FLOAT32 = "Microsoft.FSharp.Core.float32"

[<Literal>]
let FS_FLOAT64 = "Microsoft.FSharp.Core.float"

[<Literal>]
let FS_OPERATOR = "Microsoft.FSharp.Core.Operators"

[<Literal>]
let FS_OP_ADDITION = "op_Addition"

[<Literal>]
let FS_OP_SUBTRACTION = "op_Subtraction"

[<Literal>]
let FS_OP_MULTIPLY = "op_Multiply"

[<Literal>]
let FS_OP_DIVISION = "op_Division"

[<Literal>]
let FS_OP_MODULUS = "op_Modulus"

[<Literal>]
let FS_OP_EQUALITY = "op_Equality"

[<Literal>]
let FS_OP_INEQUALITY = "op_Inequality"

[<Literal>]
let FS_OP_GREATERTHAN = "op_GreaterThan"

[<Literal>]
let FS_OP_GREATERTHANOREQUAL = "op_GreaterThanOrEqual"

[<Literal>]
let FS_OP_LESSTHAN = "op_LessThan"

[<Literal>]
let FS_OP_LESSTHANOREQUAL = "op_LessThanOrEqual"

type Types =
    | SByte
    | Int16
    | Int32
    | Int64
    | Float32
    | Float64
    | Bool
    | Unit
    | Any


type FSharpDeclaration = FSharpImplementationFileDeclaration

///Contains the wasm byte representations of a F# function
type WasmFuncBytes =
    { name: string
      paramTypes: byte list
      resultType: byte
      localBytes: byte list list
      body: byte list }

type SymbolType =
    | Local
    | Param

type SymbolEntry =
    { name: string
      index: int
      typ: Types
      isUnit: bool
      symbolType: SymbolType }

type SymbolDict = Dictionary<string, SymbolEntry>

///Holds references to local params
type FunctionSymbolDict =
    { paramSymbols: Dictionary<string, SymbolEntry>
      localSymbols: Dictionary<string, SymbolEntry> }
//type LocalSymbolDict = Dictionary<string, SymbolEntry>

///This is designed to hold references to all functions inside
///a module and those functions local params
///The int in the value tuple references which numbered function
///this is in the module because Wasm tracks function by index
type ModuleSymbolDict = Dictionary<string, FunctionSymbolDict * int>

type SymbolEntries =
    | Module of ModuleSymbolDict
    | Function of FunctionSymbolDict

///Tracks all modules symbols, whether top level functions
///or local parameters
///First entry in list is always the module level functions
///Subsequent entries are local params
type ModuleSymbolList = LinkedList<SymbolEntries>
