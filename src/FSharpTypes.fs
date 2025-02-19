module TinyFS.Core.FSharpTypes

[<Literal>]
let FS_UNIT = "Microsoft.FSharp.Core.unit"

[<Literal>]
let FS_INT32 = "Microsoft.FSharp.Core.int32"

[<Literal>]
let FS_INT = "Microsoft.FSharp.Core.int"

[<Literal>]
let FS_UINT32 = "Microsoft.FSharp.Core.uint32"

// [<Literal>]
// let FS_UINT = "Microsoft.FSharp.Core.uint"

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
    | Int32
    | UInt32
    | Unit
    | Any
