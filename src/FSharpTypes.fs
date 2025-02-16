module TinyFS.Core.FSharpTypes

[<Literal>]
let FS_UNIT = "Microsoft.FSharp.Core.unit"

[<Literal>]
let FS_INT32 = "Microsoft.FSharp.Core.int"

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

type Types =
    | Int32
    | Unit
    | Any
