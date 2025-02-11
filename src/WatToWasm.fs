module Fado.Core.WatToWasm

open Fado.Core.WatAst
open System

[<Literal>]
let SECTION_ID_TYPE = 0x1uy
[<Literal>]
let SECTION_ID_FUNCTION = 0x3uy
[<Literal>]
let SECTION_ID_EXPORT = 0x7uy
[<Literal>]
let SECTION_ID_CODE = 0xAuy
[<Literal>]
let INSTR_END = 0xBuy
[<Literal>]
let i32_CONST = 0x41uy
[<Literal>]
let i64_CONST = 0x42uy
[<Literal>]
let f32_CONST = 0x43uy
[<Literal>]
let f64_CONST = 0x44uy
[<Literal>]
let TYPE_FUNCTION = 0x60uy
[<Literal>]
let INSTR_i32_ADD = 0x6Auy
[<Literal>]
let INSTR_i32_SUB = 0x6Buy
[<Literal>]
let INSTR_i32_MUL = 0x6Cuy
[<Literal>]
let INSTR_i32_DIV_S = 0x6Duy
[<Literal>]
let INSTR_i32_MOD_S = 0x6Fuy
[<Literal>]
let f64_VAL_TYPE = 0x7Cuy
[<Literal>]
let f32_VAL_TYPE = 0x7Duy
[<Literal>]
let i64_VAL_TYPE = 0x7Euy
[<Literal>]
let i32_VAL_TYPE = 0x7Fuy



let stringToBytes (s: string) =
    System.Text.Encoding.UTF8.GetBytes(s)

let int32ToBytes (v: int32) =
    let bytes = BitConverter.GetBytes(v);
    match BitConverter.IsLittleEndian with
    | true -> bytes
    | false -> Array.rev bytes

let uint32ToBytes (v: uint32) =
    let bytes = BitConverter.GetBytes(v)

    match BitConverter.IsLittleEndian with
    | true -> bytes
    | false -> Array.rev bytes

let magic () =
    // [0x00, 0x61, 0x73, 0x6d]
    let nullChar = Convert.ToChar(0).ToString()
    stringToBytes($"{nullChar}asm")

let version () =
    // [0x01, 0x00, 0x00, 0x00]
    int32ToBytes(1)

[<Literal>]
let SEVEN_BIT_MASK = 0x7f

[<Literal>]
let SEVEN_BIT_MASK_U: uint32 = 0x7fu

[<Literal>]
let SEVEN_BIT_MASK_S: int32 = 0x7f

[<Literal>]
let CONTINUATION_BIT: byte = 0x80uy

let u32 (v: uint32) =
    let mutable vall = v
    let mutable r: byte array = [||]
    let mutable more = true

    while more do
        let b: byte = (byte) (vall &&& SEVEN_BIT_MASK_U)
        vall <- vall >>> 7
        more <- vall <> 0u

        let newVall =
            if more then
                b ||| CONTINUATION_BIT
            else
                b

        r <- Array.concat [ r; [| newVall |] ]
    r

let i32 (v: int32) : byte array =
    let mutable vall = v
    let mutable r: byte array = [||]
    let mutable more = true
    let signBit = 64uy

    while more do
        let b: byte = (byte) (vall &&& SEVEN_BIT_MASK_S)
        let signBitSet = (b &&& signBit) <> 0uy

        vall <- vall >>> 7

        let nextVall =
            if ((vall = 0 && (not signBitSet))
                || (vall = -1 && signBitSet)) then
                more <- false
                b
            else
                b ||| CONTINUATION_BIT

        r <- Array.concat [ r; [| nextVall |] ]
    r

///Array helper methods
let toArr ele = [| ele |]
//Concats two arrays together
let concatArr a1 a2 =
    Array.concat [ a1; a2 ]
let concatA a1 a2 = concatArr a1 a2
//Concats one single element with an array
let concatSinArr s a =
    Array.concat [ [| s |]; a]
//Concats an array with a single element
let concatArrSin a s =
    Array.concat [ a; [| s|] ]
let section (id: byte) (contents: byte array) =
    let normalizedSize = i32 contents.Length
    let headers = concatSinArr id normalizedSize
    concatArr headers contents
let vec (elements: byte array) =
    let normalizedSize = i32 elements.Length
    concatArr normalizedSize elements

let vecFlatten (elements: byte array array) =
    let normalizedSize = i32 elements.Length
    let flattenedElements = elements
                            |> Array.collect id
    concatArr normalizedSize flattenedElements

//Type Section
let functype (paramTypes: byte array, resultTypes: byte array) =
    let paramVec = vec paramTypes
    let resultVec = vec resultTypes
    Array.concat [ [| TYPE_FUNCTION |]; paramVec; resultVec ]

let typesec (functypes : byte array array) =
    let funcVec = vecFlatten functypes
    section SECTION_ID_TYPE funcVec

//Function Section
let funcsec (typeidxs : byte array array) =
    vecFlatten typeidxs
    |> section SECTION_ID_FUNCTION

//Export section
let exportdesc (idx: byte) =
    [| 0uy; idx |]
let name (s: string) =
    s
    |> stringToBytes
    |> vec

let export (s: string) (exportDesc: byte array) =
    concatArr (name(s)) exportDesc

let exportsec (exports: byte array array) =
    vecFlatten exports
    |> section SECTION_ID_EXPORT

//Code section
let code (func: byte array) =
    let normalizedSize = i32 func.Length
    concatArr normalizedSize func

let func (locals: byte array) (body: byte array) =
    let localsVec = vec locals
    concatArr localsVec body

let codesec (codes: byte array array) =
    vecFlatten codes
    |> section SECTION_ID_CODE

let modd(sections: byte array array) =
    let flattenedSections = 
        sections
        |> Array.collect id
    Array.concat [ magic(); version(); flattenedSections ]


///Start converting functions
let operatorToWasm (op: BinaryOperator) (typ: NumberKind) =
    match op, typ with
    //arithmetic
    | BinaryPlus, Int32 -> INSTR_i32_ADD
    | BinaryMinus, Int32 -> INSTR_i32_SUB
    | BinaryMultiply, Int32 -> INSTR_i32_MUL
    | BinaryDivide, Int32 -> INSTR_i32_DIV_S
    //| "%" -> INSTR_i32_MOD_S
    //comparison
    //| "==" -> INSTR_i32_EQ
    //| "!=" -> INSTR_i32_NE
    //| "<" -> INSTR_i32_LT_S
    //| "<=" -> INSTR_i32_LE_S
    //| ">" -> INSTR_i32_GT_S
    //| ">=" -> INSTR_i32_GE_S
    ////logic
    //| "and" -> INSTR_i32_AND
    //| "or" -> INSTR_i32_OR
    //extra
    | _ -> INSTR_END

let rec exprToWasm (expr: Expr) : byte array =
    match expr with
    | Operation(kind, tags, typ) ->
        match kind, typ with
        | Binary(operator, left, right), Number(numKind)->
            let operatorWasm = 
                operatorToWasm operator numKind
                |> toArr
            let leftWasm = exprToWasm left
            let rightWasm = exprToWasm right

            Array.concat [| leftWasm; rightWasm; operatorWasm |]
        | _ -> failwith "Unsupported Operation type"
    | Value(kind) ->
        match kind with
        | NumberConstant(value) ->
            match value with
            | NumberValue.Int32 num ->  
                concatSinArr i32_CONST (i32 num)
            | _ -> failwith "Not supporting int64s right now"
        | _ -> failwith "Unsupported value type"
    | _ -> failwith "Unsupported expression type"

let rec declationToWasm (decl : Declaration) : byte array =
    match decl with
    | ModuleDeclaration modDecl ->
        declarationsToWasm modDecl.Members
    | MemberDeclaration memDecl ->
        exprToWasm memDecl.Body

and declarationsToWasm (decls : Declaration list) : byte array =
    let mutable wasmBytes : byte array = [||]

    for decl in decls do
        let ddd = declationToWasm decl
        wasmBytes <- concatArr wasmBytes ddd
    
    wasmBytes

let generateWasm (decls: Declaration list) : byte array =
    let wasmBytes = declarationsToWasm decls

    concatArrSin wasmBytes INSTR_END
//Overall compile function
let compile (decls : Declaration list) : byte array =
    let emptyBytes: byte [] = Array.zeroCreate 0

    //Creating type section
    let funcType = functype(emptyBytes, [| i32_VAL_TYPE |])
    let typeSection = typesec([| funcType |])

    //creating func section
    let funcSection = funcsec([| [|0uy|] |])

    //creating export section
    let exportDesc = exportdesc(0uy)
    let export = export "main" exportDesc
    let exportSection = exportsec [| export |]

    //creating code section
    let functions =
        generateWasm decls
        |> func emptyBytes
    let code = code functions
    let codeSection = 
        codesec [| code |]

    let sections = [| typeSection; funcSection; exportSection; codeSection |]

    let wasmBytes =
        sections
        |> modd

    wasmBytes