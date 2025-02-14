module TinyFS.Core.AstToWasm

open System
open Fable.AST
open Fable.AST.Fable
open System.Collections.Generic

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
let INSTR_CALL = 0x10uy

[<Literal>]
let INSTR_LOCAL_GET = 0x20uy

[<Literal>]
let INSTR_LOCAL_SET = 0x21uy

[<Literal>]
let INSTR_LOCAL_TEE = 0x22uy

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

///Contains the wasm byte representations of a F# function
type WasmFuncBytes =
    { name: string
      paramTypes: byte array
      resultType: byte
      locals: byte array array
      body: byte array }

type SymbolType =
    | Local
    | Param

type SymbolEntry =
    { name: string
      index: int
      symbolType: SymbolType }

///Holds references to local params
type LocalSymbolDict = Dictionary<string, SymbolEntry>

///This is designed to hold references to all functions and
///those functions local params
///The int in the value tuple references which number function
///this is in the module because Wasm tracks function by index
type FunctionSymbolDict = Dictionary<string, LocalSymbolDict * int>

type SymbolEntries =
    | Function of FunctionSymbolDict
    | Locals of LocalSymbolDict

///Tracks all modules symbols, whether top level functions
///or local parameters
///First entry in list is always the module level functions
///Subsequent entries are local params
type ModuleSymbolList = LinkedList<SymbolEntries>

let stringToBytes (s: string) = System.Text.Encoding.UTF8.GetBytes(s)

let int32ToBytes (v: int32) =
    let bytes = BitConverter.GetBytes(v)

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
    stringToBytes ($"{nullChar}asm")

let version () =
    // [0x01, 0x00, 0x00, 0x00]
    int32ToBytes (1)

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
let concatArr a1 a2 = Array.concat [ a1; a2 ]
let concatA a1 a2 = concatArr a1 a2
//Concats one single element with an array
let concatSinArr s a = Array.concat [ [| s |]; a ]
//Concats an array with a single element
let concatArrSin a s = Array.concat [ a; [| s |] ]

let locals (n: int32) (b: byte) = Array.concat [ i32 n; [| b |] ]

let section (id: byte) (contents: byte array) =
    let normalizedSize = i32 contents.Length
    let headers = concatSinArr id normalizedSize
    concatArr headers contents

let vec (elements: byte array) =
    let normalizedSize = i32 elements.Length
    concatArr normalizedSize elements

let vecFlatten (elements: byte array array) =
    let normalizedSize = i32 elements.Length
    let flattenedElements = elements |> Array.collect id
    concatArr normalizedSize flattenedElements

//Type Section
let functype (paramTypes: byte array, resultTypes: byte array) =
    let paramVec = vec paramTypes
    let resultVec = vec resultTypes

    Array.concat [ [| TYPE_FUNCTION |]
                   paramVec
                   resultVec ]

let typesec (functypes: byte array array) =
    let funcVec = vecFlatten functypes
    section SECTION_ID_TYPE funcVec

//Function Section
let funcsec (typeidxs: byte array array) =
    vecFlatten typeidxs |> section SECTION_ID_FUNCTION

//Export section
let exportdesc (idx: byte array) = Array.concat [ [| 0uy |]; idx ]
let name (s: string) = s |> stringToBytes |> vec

let export (s: string) (exportDesc: byte array) = concatArr (name (s)) exportDesc

let exportsec (exports: byte array array) =
    vecFlatten exports |> section SECTION_ID_EXPORT

//Code section
let code (func: byte array) =
    let normalizedSize = i32 func.Length
    concatArr normalizedSize func

let func (locals: byte array) (body: byte array) =
    let localsVec = vec locals
    concatArr localsVec body

let funcNested (locals: byte array array) (body: byte array) =
    let localsVec = vecFlatten locals
    Array.concat [ localsVec; body ]

let codesec (codes: byte array array) =
    vecFlatten codes |> section SECTION_ID_CODE

let modd (sections: byte array array) =
    let flattenedSections = sections |> Array.collect id

    Array.concat [ magic ()
                   version ()
                   flattenedSections ]


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

let resolveSymbols (localSymbols: LocalSymbolDict) (name: string) =
    let containsKey = localSymbols.ContainsKey name

    match containsKey with
    | true ->
        let symbol = localSymbols[name]
        Ok symbol
    | false -> Error $"Error: undeclared identifier: {name}"

let rec exprToWasm (expr: Expr) (functionSymbols: FunctionSymbolDict) (localSymbols: LocalSymbolDict) : byte array =
    match expr with
    | Operation (kind, _, typ, _) ->
        match kind, typ with
        | Binary (operator, left, right), Number (numKind, _) ->
            let operatorWasm = operatorToWasm operator numKind |> toArr
            let leftWasm = exprToWasm left functionSymbols localSymbols
            let rightWasm = exprToWasm right functionSymbols localSymbols

            Array.concat [| leftWasm
                            rightWasm
                            operatorWasm |]
        | Unary (_, operand), Number (_, _) ->
            //In F#, integer division needs to return an INT
            //So division is wrapped in an Unary expression
            //to return an INT versus a float
            //But I don't think we need that in Wasm
            exprToWasm operand functionSymbols localSymbols
        | Unary (_, operand), Any ->
            //Also used for F# integer division referenced above
            exprToWasm operand functionSymbols localSymbols
        | _ -> failwith "TinyFS: Unsupported Operation type"
    | Value (kind, _) ->
        match kind with
        | NumberConstant (value, _) ->
            match value with
            | NumberValue.Int32 num -> concatSinArr i32_CONST (i32 num)
            | _ -> failwith "TinyFS: Not supporting int64s right now"
        | _ -> failwith "TinyFS: Unsupported value type"
    | _ -> failwith "TinyFS: Unsupported expression type"

//let argsToWasm (args: Ident list) : byte array =
//    let mutable arguBytes: byte array = [||]

//    for arg in args do
//        //let argTree = expressionToWasm args symbols symbolMap
//        arguBytes <- concatArr arguBytes [||]

//    arguBytes

let rec convertToModuleSymbolList (moduleSymbolList: ModuleSymbolList) (decls: Declaration list) =
    for decl in decls do
        match decl with
        | ModuleDeclaration modDecl -> convertToModuleSymbolList moduleSymbolList modDecl.Members
        | MemberDeclaration memDecl ->
            let name = memDecl.Name
            let locals = new LocalSymbolDict()
            let last = moduleSymbolList.Last.Value

            match last with
            | Function (inner) -> inner.Add(name, (locals, inner.Count))
            | Locals _ -> failwith "TinyFS: Should have been nested when converting symbol map"

            moduleSymbolList.AddLast(Locals locals) |> ignore

            for param in memDecl.Args do
                let paramName = param.Name

                let symbolEntry =
                    { name = name
                      index = locals.Count
                      symbolType = SymbolType.Param }

                locals.Add(paramName, symbolEntry)

            //TODO - need to do this to handle local parameters inside of functions
            //Also need to figure out how to supported nested let functions
            //buildSymbolTAble memDecl.body scopes

            moduleSymbolList.RemoveLast()

            ()
        | _ -> failwith "TinyFS: Unsupported declaration type for symbolmap"

    ()

let buildModuleSymbolList (decls: Declaration list) =
    let moduleSymbolList = new ModuleSymbolList()
    let functions = Function(FunctionSymbolDict())
    moduleSymbolList.AddLast functions |> ignore

    convertToModuleSymbolList moduleSymbolList decls
    moduleSymbolList

let rec defineFunctionDecls (decls: Declaration list) (functionSymbols: FunctionSymbolDict) : WasmFuncBytes array =
    let mutable functionDecls: WasmFuncBytes array = [||]

    for decl in decls do
        let funcs =
            match decl with
            | ModuleDeclaration modDecl -> defineFunctionDecls modDecl.Members functionSymbols
            | MemberDeclaration memDecl ->
                //need logic for handling module let bindings that have no parameters
                //if that a zero parameter function? Or is that a module wide variable??
                //does it matter??
                let name = memDecl.Name

                let (symbolsOfFunction, localVars) =
                    if functionSymbols.ContainsKey name then
                        let (currentSymbols, _) = functionSymbols[name]
                        let mutable symbolValues: SymbolEntry array = [||]

                        for sym in currentSymbols.Values do
                            symbolValues <- Array.concat [ symbolValues; [| sym |] ]

                        (currentSymbols, symbolValues)
                    else
                        (new LocalSymbolDict(), [||])

                let paramVals =
                    localVars
                    |> Array.filter (fun se -> se.symbolType = SymbolType.Param)

                let paramTypes = paramVals |> Array.map (fun _ -> i32_VAL_TYPE)

                let varsCount =
                    localVars
                    |> Array.filter (fun se -> se.symbolType = SymbolType.Local)
                    |> Array.length

                let bodyWasm = exprToWasm memDecl.Body functionSymbols symbolsOfFunction

                let functionDecls: WasmFuncBytes array =
                    [| { name = name
                         paramTypes = paramTypes
                         resultType = i32_VAL_TYPE
                         locals = [| locals varsCount i32_VAL_TYPE |]
                         body =
                           Array.concat [ bodyWasm
                                          [| INSTR_END |] ] } |]

                functionDecls
            | _ -> failwith "TinyFS: Unsupported declaration type for symbolmap"

        functionDecls <- concatArr functionDecls funcs

    functionDecls

let buildModule (functionDecls: WasmFuncBytes array) =
    //creating code section
    let codeSection =
        functionDecls
        |> Array.map (fun f -> funcNested f.locals f.body)
        |> Array.map (fun f -> code f)
        |> codesec

    //Creating type section
    let typeSection =
        functionDecls
        |> Array.map (fun f -> functype (f.paramTypes, [| f.resultType |]))
        |> typesec

    //creating func section
    let funcSection =
        functionDecls
        |> Array.mapi (fun i x -> i32 i)
        |> funcsec

    //creating export section
    let exportSection =
        functionDecls
        |> Array.mapi (fun i f -> export f.name (exportdesc (i32 (i))))
        |> exportsec

    let bytes =
        modd [| typeSection
                funcSection
                exportSection
                codeSection |]

    bytes

//Overall compile function
let compile (decls: Declaration list) : byte array =
    let moduleSymbolList = buildModuleSymbolList decls

    let functionSymbols =
        match moduleSymbolList.First.Value with
        | Function fnSymbols -> fnSymbols
        | Locals _ -> failwith "TinyFS: Should not be locals in compile"

    let functionDecls = defineFunctionDecls decls functionSymbols

    buildModule functionDecls
