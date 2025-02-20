module TinyFS.Core.AstToWasm

open FSharp.Compiler.Symbols
open System
open TinyFS.Core.FSharpTypes

open System.Collections.Generic
open Utils

[<Literal>]
let SECTION_ID_TYPE = 0x1uy

[<Literal>]
let SECTION_ID_FUNCTION = 0x3uy

[<Literal>]
let SECTION_ID_EXPORT = 0x7uy

[<Literal>]
let SECTION_ID_CODE = 0xAuy


[<Literal>]
let INSTR_BLOCK = 0x2uy

[<Literal>]
let INSTR_LOOP = 0x3uy



[<Literal>]
let INSTR_IF = 0x4uy

[<Literal>]
let INSTR_ELSE = 0x5uy

[<Literal>]
let INSTR_END = 0xBuy

[<Literal>]
let INSTR_BR = 0xCuy

[<Literal>]
let INSTR_BR_IF = 0xDuy

[<Literal>]
let INSTR_CALL = 0x10uy

[<Literal>]
let INSTR_DROP = 0x1Auy

[<Literal>]
let INSTR_LOCAL_GET = 0x20uy

[<Literal>]
let INSTR_LOCAL_SET = 0x21uy

[<Literal>]
let INSTR_LOCAL_TEE = 0x22uy

[<Literal>]
let EMPTY = 0x40uy

[<Literal>]
let i32_CONST = 0x41uy

[<Literal>]
let i64_CONST = 0x42uy

[<Literal>]
let f32_CONST = 0x43uy

[<Literal>]
let f64_CONST = 0x44uy

[<Literal>]
let INSTR_i32_EQZ = 0x45uy

[<Literal>]
let INSTR_i32_EQ = 0x46uy

[<Literal>]
let INSTR_i32_NE = 0x47uy

[<Literal>]
let INSTR_i32_LT_S = 0x48uy

[<Literal>]
let INSTR_i32_LT_U = 0x49uy

[<Literal>]
let INSTR_i32_GT_S = 0x4Auy

[<Literal>]
let INSTR_i32_GT_U = 0x4Buy

[<Literal>]
let INSTR_i32_LE_S = 0x4Cuy

[<Literal>]
let INSTR_i32_LE_U = 0x4Duy

[<Literal>]
let INSTR_i32_GE_S = 0x4Euy

[<Literal>]
let INSTR_i32_GE_U = 0x4Fuy

[<Literal>]
let INSTR_i64_EQ = 0x51uy

[<Literal>]
let INSTR_i64_NE = 0x52uy

[<Literal>]
let INSTR_i64_LT_S = 0x53uy

[<Literal>]
let INSTR_i64_LT_U = 0x54uy

[<Literal>]
let INSTR_i64_GT_S = 0x55uy

[<Literal>]
let INSTR_i64_GT_U = 0x56uy

[<Literal>]
let INSTR_i64_LE_S = 0x57uy

[<Literal>]
let INSTR_i64_LE_U = 0x58uy

[<Literal>]
let INSTR_i64_GE_S = 0x59uy

[<Literal>]
let INSTR_i64_GE_U = 0x5Auy

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
let INSTR_i32_DIV_U = 0x6Euy

[<Literal>]
let INSTR_i32_MOD_S = 0x6Fuy

[<Literal>]
let INSTR_i32_MOD_U = 0x70uy

[<Literal>]
let INSTR_i64_ADD = 0x7Cuy

[<Literal>]
let INSTR_i64_SUB = 0x7Duy

[<Literal>]
let INSTR_i64_MUL = 0x7Euy

[<Literal>]
let INSTR_i64_DIV_S = 0x7Fuy

[<Literal>]
let INSTR_i64_DIV_U = 0x80uy

[<Literal>]
let INSTR_i64_MOD_S = 0x81uy

[<Literal>]
let INSTR_i64_MOD_U = 0x82uy

[<Literal>]
let f64_VAL_TYPE = 0x7Cuy

[<Literal>]
let f32_VAL_TYPE = 0x7Duy

[<Literal>]
let i64_VAL_TYPE = 0x7Euy

[<Literal>]
let i32_VAL_TYPE = 0x7Fuy

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

let stringToBytes (s: string) : byte list =
    System.Text.Encoding.UTF8.GetBytes(s)
    |> Array.toList

let int32ToBytes (v: int32) : byte list =
    let bytes = BitConverter.GetBytes(v) |> Array.toList

    match BitConverter.IsLittleEndian with
    | true -> bytes
    | false -> List.rev bytes

let uint32ToBytes (v: uint32) : byte list =
    let bytes = BitConverter.GetBytes(v) |> Array.toList

    match BitConverter.IsLittleEndian with
    | true -> bytes
    | false -> List.rev bytes

let magic () : byte list =
    // [0x00, 0x61, 0x73, 0x6d]
    let nullChar = Convert.ToChar(0).ToString()
    stringToBytes ($"{nullChar}asm")

let version () : byte list =
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

let u32 (v: uint32) : byte list =
    let mutable vall = v
    let mutable r: byte list = []
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

        r <- r @ [ newVall ]

    r

let i64 (v: int64) : byte list =
    let mutable vall = v
    let mutable r: byte list = []
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

        r <- r @ [ nextVall ]

    r

let i32 (v: int32) : byte list =
    let mutable vall = v
    let mutable r: byte list = []
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

        r <- r @ [ nextVall ]

    r

let i8 (v: sbyte) = System.Convert.ToInt32(v) |> i32
let i16 (v: int16) = System.Convert.ToInt32(v) |> i32

///List helper methods
let toList ele = [ ele ]
///Append two list together
let appendList a1 a2 = a1 @ a2
let aList a1 a2 = appendList a1 a2
///Append single element with an list
let appendSinList s a = [ s ] @ a
///Append alist with a single element
let appendListSin a s = a @ [ s ]
///Append to List with 2 parameters
let aList2 l1 l2 = l1 @ l2
///Append to List with 3 parameters
let aList3 l1 l2 l3 = l1 @ l2 @ l3
///Append to List with 4 parameters
let aList4 l1 l2 l3 l4 = l1 @ l2 @ l3 @ l4
///Append to List with 5 parameters
let aList5 l1 l2 l3 l4 l5 = l1 @ l2 @ l3 @ l4 @ l5
///Append to List with 6 parameters
let aList6 l1 l2 l3 l4 l5 l6 = l1 @ l2 @ l3 @ l4 @ l5 @ l6

let locals (n: int32) (b: byte) = (i32 n) @ [ b ]

let section (id: byte) (contents: byte list) =
    let normalizedSize = i32 contents.Length
    let headers = appendSinList id normalizedSize
    appendList headers contents

let vec (elements: byte list) =
    let normalizedSize = i32 elements.Length
    appendList normalizedSize elements

let vecFlatten (elements: byte list list) =
    let normalizedSize = i32 elements.Length
    let flattenedElements = elements |> List.collect id
    appendList normalizedSize flattenedElements

//Type Section
let functype (paramTypes: byte list, resultTypes: byte list) =
    let paramVec = vec paramTypes
    let resultVec = vec resultTypes

    aList3 [ TYPE_FUNCTION ] paramVec resultVec

let typesec (functypes: byte list list) =
    let funcVec = vecFlatten functypes
    section SECTION_ID_TYPE funcVec

//Function Section
let funcsec (typeidxs: byte list list) =
    vecFlatten typeidxs |> section SECTION_ID_FUNCTION

//Export section
let exportdesc (idx: byte list) = appendSinList 0uy idx
let name (s: string) = s |> stringToBytes |> vec

let export (s: string) (exportDesc: byte list) = appendList (name (s)) exportDesc

let exportsec (exports: byte list list) =
    vecFlatten exports |> section SECTION_ID_EXPORT

//Code section
let code (func: byte list) =
    let normalizedSize = i32 func.Length
    appendList normalizedSize func

let func (locals: byte list) (body: byte list) =
    let localsVec = vec locals
    appendList localsVec body

let funcNested (locals: byte list list) (body: byte list) =
    let localsVec = vecFlatten locals
    aList2 localsVec body

let codesec (codes: byte list list) =
    vecFlatten codes |> section SECTION_ID_CODE

let modd (sections: byte list list) =
    let flattenedSections = sections |> List.collect id

    aList3 (magic ()) (version ()) flattenedSections

type blockType =
    | Empty_
    | I32
    | I64
    | F32
    | F64

let getBlockType bType =
    match bType with
    | Empty_ -> EMPTY
    | I32 -> i32_VAL_TYPE
    | I64 -> i64_VAL_TYPE
    | F32 -> f32_VAL_TYPE
    | F64 -> f64_VAL_TYPE

let getResultType typ =
    match typ with
    | Types.SByte
    | Types.Int16
    | Types.Int32 -> i32_VAL_TYPE
    | Types.Int64 -> i64_VAL_TYPE
    | _ -> tinyfail (sprintf "Currently does not support '%s' result type" (typ.ToString()))

let getType (strType) =
    match strType with
    | FS_SBYTE -> Types.SByte
    | FS_INT16 -> Types.Int16
    | FS_INT
    | FS_INT32 -> Types.Int32
    | FS_INT64 -> Types.Int64
    | FS_BOOL -> Types.Bool
    | FS_UNIT -> Types.Unit
    | _ -> tinyfail (sprintf "'%s' is an unknown type" strType)

let getOperatorType typ =
    match typ with
    | Types.Bool
    | Types.SByte
    | Types.Int16
    | Types.Int32 -> Types.Int32
    | Types.Int64 -> Types.Int64
    | _ -> tinyfail (sprintf "Currently does not support '%s' operator type" (typ.ToString()))

///Start converting functions
let operatorToWasm (op: string) (typ: Types) =
    match op, typ with
    //arithmetic
    | FS_OP_ADDITION, Int32 -> INSTR_i32_ADD
    | FS_OP_ADDITION, Int64 -> INSTR_i64_ADD
    | FS_OP_SUBTRACTION, Int32 -> INSTR_i32_SUB
    | FS_OP_SUBTRACTION, Int64 -> INSTR_i64_SUB
    | FS_OP_MULTIPLY, Int32 -> INSTR_i32_MUL
    | FS_OP_MULTIPLY, Int64 -> INSTR_i64_MUL
    | FS_OP_DIVISION, Int32 -> INSTR_i32_DIV_S
    | FS_OP_DIVISION, Int64 -> INSTR_i64_DIV_S
    | FS_OP_MODULUS, Int32 -> INSTR_i32_MOD_S
    | FS_OP_MODULUS, Int64 -> INSTR_i64_MOD_S
    //comparison
    | FS_OP_EQUALITY, Int32 -> INSTR_i32_EQ
    | FS_OP_EQUALITY, Int64 -> INSTR_i64_EQ
    | FS_OP_INEQUALITY, Int32 -> INSTR_i32_NE
    | FS_OP_INEQUALITY, Int64 -> INSTR_i64_NE
    | FS_OP_LESSTHAN, Int32 -> INSTR_i32_LT_S
    | FS_OP_LESSTHAN, Int64 -> INSTR_i64_LT_S
    | FS_OP_LESSTHANOREQUAL, Int32 -> INSTR_i32_LE_S
    | FS_OP_LESSTHANOREQUAL, Int64 -> INSTR_i64_LE_S
    | FS_OP_GREATERTHAN, Int32 -> INSTR_i32_GT_S
    | FS_OP_GREATERTHAN, Int64 -> INSTR_i64_GT_S
    | FS_OP_GREATERTHANOREQUAL, Int32 -> INSTR_i32_GE_S
    | FS_OP_GREATERTHANOREQUAL, Int64 -> INSTR_i64_GE_S
    ////logic
    //| "and" -> INSTR_i32_AND
    //| "or" -> INSTR_i32_OR
    //extra
    | opName, typeName ->
        failwith (
            sprintf
                "TinyFS: We currently do not support the '%s' operator with the '%s' type"
                opName
                (typeName.ToString())
        )

let resolveLocalSymbols (functionSymbols: FunctionSymbolDict) (name: string) =
    let isLocal = functionSymbols.localSymbols.ContainsKey name

    match isLocal with
    | true -> functionSymbols.localSymbols[name]
    | false ->
        let isParam = functionSymbols.paramSymbols.ContainsKey name

        match isParam with
        | true -> functionSymbols.paramSymbols[name]
        | false -> failwith $"TinyFS: undeclared identifier: {name}"

///Rethink this approach
let getLocalIndex (functionSymbols: FunctionSymbolDict) (sym: SymbolEntry) =
    match sym.symbolType with
    | SymbolType.Param -> sym.index
    | SymbolType.Local ->
        let nonUnitParamCount =
            functionSymbols.paramSymbols
            |> Seq.filter (fun keyVP -> not keyVP.Value.isUnit)
            |> Seq.length

        nonUnitParamCount + sym.index

let buildDeclError decl =
    (sprintf "TinyFS: '%s' is currently an unsupported declaration type" (decl.GetType().ToString()))

let buildExprError (expr: FSharpExpr) =
    let exprType = determineExprPattern expr
    (sprintf "TinyFS: '%s+%s' is currently an unsupported expression type" (expr.GetType().ToString()) exprType)

let private isOperator (memb: FSharpMemberOrFunctionOrValue) =
    match memb.DeclaringEntity with
    | Some de ->
        match de.BasicQualifiedName with
        | FS_OPERATOR -> (true, memb.CompiledName)
        | _ -> (false, "")
    | _ -> (false, "")

let rec exprToWasm
    (expr: FSharpExpr)
    (moduleSymbols: ModuleSymbolDict)
    (functionSymbols: FunctionSymbolDict)
    : byte list =
    match expr with
    | FSharpExprPatterns.Call (callExpr, memb, ownerGenArgs, memberGenArgs, args) ->
        match isOperator memb with
        | true, opName ->
            match args.Length with
            | 2 ->
                let exprType =
                    getType expr.Type.BasicQualifiedName
                    |> getOperatorType

                let opWasm = operatorToWasm opName exprType |> toList

                let leftWasm = exprToWasm args[0] moduleSymbols functionSymbols
                let rightWasm = exprToWasm args[1] moduleSymbols functionSymbols
                aList3 leftWasm rightWasm opWasm
            | _ ->
                failwith (
                    sprintf "TinyFS: Was not expecetd %d argument(s) with '%s' operator" args.Length memb.CompiledName
                )
        | _ ->
            let index =
                if moduleSymbols.ContainsKey memb.CompiledName then
                    let (_, idx) = moduleSymbols[memb.CompiledName]
                    idx
                else
                    failwith (sprintf "TinyFS: Cannot find function '%s' in FunctionSymbols table" memb.CompiledName)

            let argumentBytes =
                args
                |> List.filter (fun arg ->
                    (getType arg.Type.BasicQualifiedName)
                    <> Types.Unit)
                |> List.map (fun arg -> exprToWasm arg moduleSymbols functionSymbols)
                |> List.collect id

            let callWasm = appendSinList INSTR_CALL (i32 index)

            let wasmBytes = aList2 argumentBytes callWasm

            wasmBytes
    | FSharpExprPatterns.Const (value, typ) ->
        let typ = getType typ.BasicQualifiedName

        match typ with
        | Types.Int32 ->
            match convertInt value with
            | Some vall -> appendSinList i32_CONST (i32 vall)
            | None -> failwith (sprintf "TinyFS: Cannot convert '%s' to Int32" (value.ToString()))
        | Types.SByte ->
            match convertSByte value with
            | Some vall -> appendSinList i32_CONST (i8 vall)
            | None -> tinyfail (sprintf "Cannot convert '%s' to SByte" (value.ToString()))
        | Types.Int16 ->
            match convertInt16 value with
            | Some vall -> appendSinList i32_CONST (i16 vall)
            | None -> tinyfail (sprintf "Cannot convert '%s' to Int16" (value.ToString()))
        | Types.Int64 ->
            match convertInt64 value with
            | Some vall -> appendSinList i64_CONST (i64 vall)
            | None -> tinyfail (sprintf "Cannot convert '%s' to Int64" (value.ToString()))
        | _ -> tinyfail (sprintf "Cannot extract value from '%s' type" (typ.ToString()))
    | FSharpExprPatterns.Let ((vall, letExpr, _), rightExpr) ->
        let leftWasm = exprToWasm letExpr moduleSymbols functionSymbols
        let rightWasm = exprToWasm rightExpr moduleSymbols functionSymbols

        let localIndex =
            resolveLocalSymbols functionSymbols vall.CompiledName
            |> getLocalIndex functionSymbols

        let localWasm = appendSinList INSTR_LOCAL_SET (i32 localIndex)

        aList3 leftWasm localWasm rightWasm
    | FSharpExprPatterns.Value (vall) ->
        let localIndex =
            resolveLocalSymbols functionSymbols vall.CompiledName
            |> getLocalIndex functionSymbols

        appendSinList INSTR_LOCAL_GET (i32 localIndex)
    | FSharpExprPatterns.IfThenElse (guardExpr, thenExpr, elseExpr) ->
        let guardWasm = exprToWasm guardExpr moduleSymbols functionSymbols
        let ifCommandWasm = [ INSTR_IF; getBlockType I32 ]
        let thenWasm = exprToWasm thenExpr moduleSymbols functionSymbols
        let elseCommandWasm = [ INSTR_ELSE ]
        let elseWasm = exprToWasm elseExpr moduleSymbols functionSymbols
        let endingWasm = [ INSTR_END ]

        let wasmBytes =
            aList6 guardWasm ifCommandWasm thenWasm elseCommandWasm elseWasm endingWasm

        wasmBytes
    | FSharpExprPatterns.Sequential (first, second) ->
        let firstWasm = exprToWasm first moduleSymbols functionSymbols
        let secondWasm = exprToWasm second moduleSymbols functionSymbols
        aList2 firstWasm secondWasm
    | FSharpExprPatterns.ValueSet (valToSet, valueExpr) ->
        let symbolIndex =
            resolveLocalSymbols functionSymbols valToSet.CompiledName
            |> getLocalIndex functionSymbols

        let exprWasm = exprToWasm valueExpr moduleSymbols functionSymbols

        let setWasm = aList3 [ INSTR_LOCAL_TEE ] (i32 symbolIndex) [ INSTR_DROP ]

        aList2 exprWasm setWasm
    | FSharpExprPatterns.WhileLoop (guardExpr, bodyExpr, _) ->
        let loopWasm = [ INSTR_LOOP; (getBlockType Empty_) ]
        let guardWasm = exprToWasm guardExpr moduleSymbols functionSymbols
        let ifWasm = [ INSTR_IF; (getBlockType Empty_) ]
        let bodyWasm = exprToWasm bodyExpr moduleSymbols functionSymbols
        let brWasm = aList3 [ INSTR_BR ] (i32 1) [ INSTR_END; INSTR_END ]

        let wasmBytes = aList5 loopWasm guardWasm ifWasm bodyWasm brWasm
        wasmBytes
    | _ -> failwith (buildExprError expr)

let rec defineFunctionDecls decls (moduleSymbols: ModuleSymbolDict) : WasmFuncBytes list =
    let mutable functionDecls: WasmFuncBytes list = []

    for decl in decls do
        let funcs =
            match decl with
            | FSharpDeclaration.MemberOrFunctionOrValue (vall, curriedArgs, body) when vall.IsFunction ->
                //need logic for handling module let bindings that have no parameters
                //if that a zero parameter function? Or is that a module wide variable??
                //does it matter??
                let name = vall.CompiledName

                let (functionSymbols, idx) =

                    if moduleSymbols.ContainsKey name then
                        moduleSymbols[name]
                    else
                        failwith (sprintf "TinyFS: Cannot find symbolDict for %s" name)

                let paramTypes =
                    functionSymbols.paramSymbols
                    |> Seq.map (fun keyVP -> keyVP.Value)
                    |> Seq.filter (fun sym -> not sym.isUnit)
                    |> Seq.sortBy (fun sym -> sym.index)
                    |> Seq.map (fun sym -> getResultType sym.typ)
                    |> Seq.toList

                let varsCount =
                    functionSymbols.localSymbols
                    |> Seq.map (fun keyVP -> keyVP.Value)
                    |> Seq.sortBy (fun sym -> sym.index)
                    |> Seq.length

                let bodyWasm = exprToWasm body moduleSymbols functionSymbols

                let resultType =
                    body.Type.BasicQualifiedName
                    |> getType
                    |> getResultType


                let functionDecls: WasmFuncBytes list =
                    [ { name = name
                        paramTypes = paramTypes
                        resultType = resultType
                        localBytes = [ locals varsCount i32_VAL_TYPE ]
                        body = appendListSin bodyWasm INSTR_END } ]

                functionDecls
            | FSharpDeclaration.MemberOrFunctionOrValue (_, _, _) ->
                failwith "TinyFS: Currently do not support module level members"
            | FSharpDeclaration.Entity (_, decls) -> defineFunctionDecls decls moduleSymbols
            | _ -> failwith (buildDeclError decl)

        functionDecls <- appendList functionDecls funcs

    functionDecls

let rec exprToSymbolList (functionSymbols: FunctionSymbolDict) expr =
    match expr with
    | FSharpExprPatterns.Let ((vall, letExpr, _), rightExpr) ->
        let name = vall.CompiledName

        let typ = getType vall.FullType.BasicQualifiedName

        let symbolEntry =
            { name = name
              index = functionSymbols.localSymbols.Count
              isUnit = typ = Types.Unit
              typ = typ
              symbolType = SymbolType.Local }

        functionSymbols.localSymbols.Add(name, symbolEntry)

        exprToSymbolList functionSymbols letExpr
        exprToSymbolList functionSymbols rightExpr
    | FSharpExprPatterns.Sequential (first, second) ->
        exprToSymbolList functionSymbols first
        exprToSymbolList functionSymbols second
    | FSharpExprPatterns.WhileLoop (guardExpr, bodyExpr, _) ->
        exprToSymbolList functionSymbols guardExpr
        exprToSymbolList functionSymbols bodyExpr
    | _ -> ()

let rec convertToModuleSymbolList (moduleSymbolList: ModuleSymbolList) decls =
    for decl in decls do
        match decl with
        | FSharpDeclaration.MemberOrFunctionOrValue (vall, curriedArgs, body) when vall.IsFunction ->
            let name = vall.CompiledName

            let functionSymbols =
                { localSymbols = new SymbolDict()
                  paramSymbols = new SymbolDict() }

            let last = moduleSymbolList.Last.Value

            match last with
            | Module (mods) -> mods.Add(name, (functionSymbols, mods.Count))
            | Function _ -> failwith "TinyFS: Should have been Funcion in ConvertingSymbolMap"

            moduleSymbolList.AddLast(Function functionSymbols)
            |> ignore

            for args in curriedArgs do
                for arg in args do
                    let paramName = arg.CompiledName
                    let typ = getType arg.FullType.BasicQualifiedName

                    let symbolEntry =
                        { name = name
                          index = functionSymbols.paramSymbols.Count
                          isUnit = typ = Types.Unit
                          typ = typ
                          symbolType = SymbolType.Param }

                    functionSymbols.paramSymbols.Add(paramName, symbolEntry)

            exprToSymbolList functionSymbols body

            moduleSymbolList.RemoveLast()
        | FSharpDeclaration.MemberOrFunctionOrValue (_, _, _) ->
            failwith "TinyFS: Currently do not support module level members"
        | FSharpDeclaration.Entity (_, decls) -> convertToModuleSymbolList moduleSymbolList decls
        | _ -> failwith "TinyFS: Unsupported declaration type for symbolmap"

    ()

let buildModuleSymbolList (decls: FSharpImplementationFileDeclaration list) =
    let moduleSymbolList = new ModuleSymbolList()
    let functions = Module(ModuleSymbolDict())
    moduleSymbolList.AddLast functions |> ignore

    convertToModuleSymbolList moduleSymbolList decls
    moduleSymbolList

let buildModule (functionDecls: WasmFuncBytes list) : byte list =
    //creating code section
    let codeSection =
        functionDecls
        |> List.map (fun f -> funcNested f.localBytes f.body)
        |> List.map (fun f -> code f)
        |> codesec

    //Creating type section
    let typeSection =
        functionDecls
        |> List.map (fun f -> functype (f.paramTypes, [ f.resultType ]))
        |> typesec

    //creating func section
    let funcSection =
        functionDecls
        |> List.mapi (fun i x -> i32 i)
        |> funcsec

    //creating export section
    let exportSection =
        functionDecls
        |> List.mapi (fun i f -> export f.name (exportdesc (i32 (i))))
        |> exportsec

    let bytes =
        modd [ typeSection
               @ funcSection @ exportSection @ codeSection ]

    bytes

let private getFunctionSymbols (moduleSymbolList: ModuleSymbolList) =
    match moduleSymbolList.First.Value with
    | Module fnSymbols -> fnSymbols
    | Function _ -> failwith "TinyFS: Should not be locals in AstToWasm.getFunctionSymbols"

let confirmMainFunction (moduleSymbolList: ModuleSymbolList) =
    let mainName = "main"

    match moduleSymbolList.First.Value with
    | Module modd ->
        if modd.ContainsKey mainName then
            let (functionDict, _) = modd[mainName]

            let nonUnitParamCount =
                functionDict.paramSymbols
                |> Seq.filter (fun keyVP -> not keyVP.Value.isUnit)
                |> Seq.length

            if nonUnitParamCount = 0 then
                moduleSymbolList
            else
                failwith "TinyFS currently requires a zero parameter 'main' function to exist"
        else
            failwith "TinyFS requires a zero parameter 'main' function to exist"
    | Function _ -> failwith "Should not be functions"

let astToWasm (decls: FSharpImplementationFileDeclaration list) =
    buildModuleSymbolList decls
    |> confirmMainFunction
    |> getFunctionSymbols
    |> defineFunctionDecls decls
    |> buildModule
