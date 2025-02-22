module TinyFS.Core.AstToWasm

open FSharp.Compiler.Symbols
open System
open TinyFS.Core.TypeInfos
open TinyFS.Core.WasmLiterals.Instructions
open TinyFS.Core.WasmLiterals.Section
open TinyFS.Core.Utils

let magic () : byte list =
    // [0x00, 0x61, 0x73, 0x6d]
    let nullChar = Convert.ToChar(0).ToString()
    stringToBytes ($"{nullChar}asm")

let version () : byte list =
    // [0x01, 0x00, 0x00, 0x00]
    int32ToBytes (1)

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

let getBlockType (typ: Types option) =
    match typ with
    | None -> EMPTY
    | Some vall ->
        match vall with
        | Bool
        | Int32 -> i32_VAL_TYPE
        | Int64 -> i64_VAL_TYPE
        //| F32 -> f32_VAL_TYPE
        //| F64 -> f64_VAL_TYPE
        | _ -> tinyfail (sprintf "Currently do not support a '%s' block type" (vall.ToString()))

let getResultType typ =
    match typ with
    | Types.Bool
    | Types.SByte
    | Types.Int16
    | Types.Int32 -> i32_VAL_TYPE
    | Types.Int64 -> i64_VAL_TYPE
    | Types.Float64 -> f64_VAL_TYPE
    | _ -> tinyfail (sprintf "Currently does not support '%s' result type" (typ.ToString()))

let getType (strType) =
    match strType with
    | FS_SBYTE -> Types.SByte
    | FS_INT16 -> Types.Int16
    | FS_INT
    | FS_INT32 -> Types.Int32
    | FS_INT64 -> Types.Int64
    | FS_FLOAT64 -> Types.Float64
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
    | Types.Float64 -> Types.Float64
    | _ -> tinyfail (sprintf "Currently does not support '%s' operator type" (typ.ToString()))

let getOperatorTypeFromArgs (arg1: FSharpExpr) (arg2: FSharpExpr) =
    let arg1Type = getType arg1.Type.BasicQualifiedName
    let arg2Type = getType arg2.Type.BasicQualifiedName

    if arg1Type = arg2Type then
        getOperatorType arg1Type
    else
        tinyfail (sprintf "Cannot deduce operator for '%s' and '%s' types" (arg1Type.ToString()) (arg2Type.ToString()))

///Start converting functions
let operatorToWasm (op: string) (typ: Types) =
    match op, typ with
    //arithmetic
    | FS_OP_ADDITION, Int32 -> INSTR_i32_ADD
    | FS_OP_ADDITION, Int64 -> INSTR_i64_ADD
    | FS_OP_ADDITION, Float64 -> INSTR_f64_ADD
    | FS_OP_SUBTRACTION, Int32 -> INSTR_i32_SUB
    | FS_OP_SUBTRACTION, Int64 -> INSTR_i64_SUB
    | FS_OP_SUBTRACTION, Float64 -> INSTR_f64_SUB
    | FS_OP_MULTIPLY, Int32 -> INSTR_i32_MUL
    | FS_OP_MULTIPLY, Int64 -> INSTR_i64_MUL
    | FS_OP_MULTIPLY, Float64 -> INSTR_f64_MUL
    | FS_OP_DIVISION, Int32 -> INSTR_i32_DIV_S
    | FS_OP_DIVISION, Int64 -> INSTR_i64_DIV_S
    | FS_OP_DIVISION, Float64 -> INSTR_f64_DIV
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
        tinyfail (
            sprintf "We currently do not support the '%s' operator with the '%s' type" opName (typeName.ToString())
        )

let resolveLocalSymbols (functionSymbols: FunctionSymbolDict) (name: string) =
    let isLocal = functionSymbols.localSymbols.ContainsKey name

    match isLocal with
    | true -> functionSymbols.localSymbols[name]
    | false ->
        let isParam = functionSymbols.paramSymbols.ContainsKey name

        match isParam with
        | true -> functionSymbols.paramSymbols[name]
        | false -> tinyfail $"undeclared identifier: {name}"

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
    (sprintf "'%s' is currently an unsupported declaration type" (decl.GetType().ToString()))

let buildExprError (expr: FSharpExpr) =
    let exprType = determineExprPattern expr
    (sprintf "'%s+%s' is currently an unsupported expression type" (expr.GetType().ToString()) exprType)

let private isOperator (memb: FSharpMemberOrFunctionOrValue) =
    match memb.DeclaringEntity with
    | Some de ->
        match de.BasicQualifiedName with
        | FS_OPERATOR -> (true, memb.CompiledName)
        | _ -> (false, "")
    | _ -> (false, "")

//I first wrote this when supporting boolean expressions.
//I needed a way to convert from bool -> i32/i64 when comparing
//a more complicated boolean expression (1 > 2 && num1). Now that
//I support proper boolean types, this might not be needed anymore.
let normalizeWasmToType (typ: Types) (wasm: byte list) =
    match typ, wasm.Length with
    | Types.SByte, 1
    | Types.Int16, 1
    | Types.Int32, 1 ->
        let intVal = if wasm[0] = 0uy then 0 else 1
        appendSinList i32_CONST (i32 intVal)
    | Types.Int32, 2 -> wasm
    | Types.Int64, 1 ->
        let intVal = if wasm[0] = 0uy then 0L else 1L
        appendSinList i64_CONST (i64 intVal)
    | _, _ ->
        tinyfail (sprintf "Do not know how to normalize type of '%s' with length '%d'" (typ.ToString()) wasm.Length)

///Turns out that the F# AST does not convert boolean operators
///to function calls. So instead of '&&' or '||' call some function
///to calculate a value, it's encoded in the AST itself.
///ie: `if (n = 0 && n = 0) then`, the F# AST has a nested IfThen structure
///with an orphan bool type. In order to tell what type that bool should
///be (int32/int64), This function normalizes the orphan bool branch with
///the same type as the non-orphan branch
let private normalizeBranchReturns (thenType: Types) (thenWasm: byte list) (elseType: Types) (elseWasm: byte list) =
    if thenType = elseType then
        (thenWasm, elseWasm)
    else if thenType = Types.Bool then
        let wasm = normalizeWasmToType elseType thenWasm
        (wasm, elseWasm)
    else
        let wasm = normalizeWasmToType thenType elseWasm
        (thenWasm, wasm)

///The fact that a i64 operator returns an i32 means we can't always rely
///on the argument types to know what the return type is. We can't always
///default to i32, since sometimes we need an i64, and vice versa.
///So this logic helps us determine which type we really need to return
let private normalizeTypeReturn (guardType: Types) (thenType: Types) (elseType: Types) =
    if thenType = Types.Bool || elseType = Types.Bool then
        guardType
    elif thenType <> elseType then
        guardType
    else
        thenType

let rec exprToWasm
    (expr: FSharpExpr)
    (moduleSymbols: ModuleSymbolDict)
    (functionSymbols: FunctionSymbolDict)
    : Types * (byte list) =
    match expr with
    | FSharpExprPatterns.Call (callExpr, memb, ownerGenArgs, memberGenArgs, args) ->
        match isOperator memb with
        | true, opName ->
            match args.Length with
            | 2 ->
                let exprType = getOperatorTypeFromArgs args[0] args[1]
                //getType expr.Type.BasicQualifiedName
                //|> getOperatorType

                let opWasm = operatorToWasm opName exprType |> toList

                let (_, leftWasm) = exprToWasm args[0] moduleSymbols functionSymbols
                let (_, rightWasm) = exprToWasm args[1] moduleSymbols functionSymbols

                (Types.Int32, (aList3 leftWasm rightWasm opWasm))
            | _ -> tinyfail (sprintf "Was not expecetd %d argument(s) with '%s' operator" args.Length memb.CompiledName)
        | _ ->
            let index =
                if moduleSymbols.ContainsKey memb.CompiledName then
                    let (_, idx) = moduleSymbols[memb.CompiledName]
                    idx
                else
                    tinyfail (sprintf "Cannot find function '%s' in FunctionSymbols table" memb.CompiledName)

            let argumentWasm =
                args
                |> List.filter (fun arg ->
                    (getType arg.Type.BasicQualifiedName)
                    <> Types.Unit)
                |> List.map (fun arg -> exprToWasm arg moduleSymbols functionSymbols)
                |> List.map (fun (typ, bytes) -> bytes)
                |> List.collect id

            let callWasm = appendSinList INSTR_CALL (i32 index)

            let functionCallWasm = aList2 argumentWasm callWasm

            //TODO - returning this as a Unit might be a bug
            (Types.Unit, functionCallWasm)
    | FSharpExprPatterns.Const (value, typ) ->
        let typ = getType typ.BasicQualifiedName

        match typ with
        | Types.SByte ->
            match convertSByte value with
            | Some vall -> (Types.Int32, (appendSinList i32_CONST (i8 vall)))
            | None -> tinyfail (sprintf "Cannot convert '%s' to SByte" (value.ToString()))
        | Types.Int16 ->
            match convertInt16 value with
            | Some vall -> (Types.Int32, (appendSinList i32_CONST (i16 vall)))
            | None -> tinyfail (sprintf "Cannot convert '%s' to Int16" (value.ToString()))
        | Types.Int32 ->
            match convertInt value with
            | Some vall -> (Types.Int32, (appendSinList i32_CONST (i32 vall)))
            | None -> tinyfail (sprintf "Cannot convert '%s' to Int32" (value.ToString()))
        | Types.Int64 ->
            match convertInt64 value with
            | Some vall -> (Types.Int64, (appendSinList i64_CONST (i64 vall)))
            | None -> tinyfail (sprintf "Cannot convert '%s' to Int64" (value.ToString()))
        | Types.Float64 ->
            match convertFloat64 value with
            | Some vall ->
                let floatBytes = BitConverter.GetBytes(vall) |> Array.toList
                (Types.Float64, (appendSinList f64_CONST floatBytes))
            | None -> tinyfail (sprintf "Cannot convert '%s' to Float64" (value.ToString()))
        | Types.Bool ->
            match convertBool value with
            | Some vall ->
                let vallByte = if vall then 1uy else 0uy
                (Types.Bool, (appendSinList i32_CONST [ vallByte ]))
            | None -> tinyfail (sprintf "Cannot convert '%s' to Bool" (value.ToString()))
        | _ -> tinyfail (sprintf "Cannot extract value from '%s' type" (typ.ToString()))
    | FSharpExprPatterns.Let ((vall, letExpr, _), rightExpr) ->
        let (_, leftWasm) = exprToWasm letExpr moduleSymbols functionSymbols
        let (_, rightWasm) = exprToWasm rightExpr moduleSymbols functionSymbols

        let localSymbol = resolveLocalSymbols functionSymbols vall.CompiledName

        let localIndex = getLocalIndex functionSymbols localSymbol

        let localWasm = appendSinList INSTR_LOCAL_SET (i32 localIndex)

        (localSymbol.typ, (aList3 leftWasm localWasm rightWasm))
    | FSharpExprPatterns.Value (vall) ->
        let localSymbol = resolveLocalSymbols functionSymbols vall.CompiledName

        let localIndex = getLocalIndex functionSymbols localSymbol

        (localSymbol.typ, (appendSinList INSTR_LOCAL_GET (i32 localIndex)))
    | FSharpExprPatterns.IfThenElse (guardExpr, thenExpr, elseExpr) ->
        let (guardTyp, guardWasm) = exprToWasm guardExpr moduleSymbols functionSymbols

        let (thenTyp, thenTempWasm) = exprToWasm thenExpr moduleSymbols functionSymbols
        let elseCommandWasm = [ INSTR_ELSE ]
        let (elseTyp, elseTempWasm) = exprToWasm elseExpr moduleSymbols functionSymbols

        let (thenWasm, elseWasm) =
            normalizeBranchReturns thenTyp thenTempWasm elseTyp elseTempWasm

        let endingWasm = [ INSTR_END ]

        let returnType = normalizeTypeReturn guardTyp thenTyp elseTyp

        let ifCommandWasm =
            [ INSTR_IF
              getBlockType (Some returnType) ]

        let wasmBytes =
            aList6 guardWasm ifCommandWasm thenWasm elseCommandWasm elseWasm endingWasm

        (returnType, wasmBytes)
    | FSharpExprPatterns.Sequential (first, second) ->
        let (firstTyp, firstWasm) = exprToWasm first moduleSymbols functionSymbols
        let (secondTyp, secondWasm) = exprToWasm second moduleSymbols functionSymbols
        (firstTyp, (aList2 firstWasm secondWasm))
    | FSharpExprPatterns.ValueSet (valToSet, valueExpr) ->
        let localSymbol = resolveLocalSymbols functionSymbols valToSet.CompiledName
        let localIndex = getLocalIndex functionSymbols localSymbol

        let (exprTyp, exprWasm) = exprToWasm valueExpr moduleSymbols functionSymbols

        let setWasm = aList3 [ INSTR_LOCAL_TEE ] (i32 localIndex) [ INSTR_DROP ]

        (localSymbol.typ, (aList2 exprWasm setWasm))
    | FSharpExprPatterns.WhileLoop (guardExpr, bodyExpr, _) ->
        let loopWasm = [ INSTR_LOOP; (getBlockType None) ]
        let (guardTyp, guardWasm) = exprToWasm guardExpr moduleSymbols functionSymbols
        let ifWasm = [ INSTR_IF; (getBlockType None) ]
        let (bodyTyp, bodyWasm) = exprToWasm bodyExpr moduleSymbols functionSymbols
        let brWasm = aList3 [ INSTR_BR ] (i32 1) [ INSTR_END; INSTR_END ]

        let wasmBytes = aList5 loopWasm guardWasm ifWasm bodyWasm brWasm
        (bodyTyp, wasmBytes)
    | _ -> tinyfail (buildExprError expr)

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
                        tinyfail (sprintf "Cannot find symbolDict for %s" name)

                let paramWasm =
                    functionSymbols.paramSymbols
                    |> Seq.map (fun keyVP -> keyVP.Value)
                    |> Seq.filter (fun sym -> not sym.isUnit)
                    |> Seq.sortBy (fun sym -> sym.index)
                    |> Seq.map (fun sym -> getResultType sym.typ)
                    |> Seq.toList

                //could probably optimize this
                //instead of a in order mapping, group by
                //type then map to locals
                let localWasm =
                    functionSymbols.localSymbols
                    |> Seq.map (fun keyVP -> keyVP.Value)
                    |> Seq.sortBy (fun sym -> sym.index)
                    |> Seq.map (fun sym -> [ locals 1 (getResultType sym.typ) ])
                    |> List.concat

                let (bodyTyp, bodyWasm) = exprToWasm body moduleSymbols functionSymbols

                let resultType =
                    body.Type.BasicQualifiedName
                    |> getType
                    |> getResultType


                let functionDecls: WasmFuncBytes list =
                    [ { name = name
                        paramTypes = paramWasm
                        resultType = resultType
                        localBytes = localWasm
                        body = appendListSin bodyWasm INSTR_END } ]

                functionDecls
            | FSharpDeclaration.MemberOrFunctionOrValue (_, _, _) ->
                tinyfail "Currently do not support module level members"
            | FSharpDeclaration.Entity (_, decls) -> defineFunctionDecls decls moduleSymbols
            | _ -> tinyfail (buildDeclError decl)

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
            | Function _ -> tinyfail "Should have been Funcion in ConvertingSymbolMap"

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
            tinyfail "Currently do not support module level members"
        | FSharpDeclaration.Entity (_, decls) -> convertToModuleSymbolList moduleSymbolList decls
        | _ -> tinyfail "Unsupported declaration type for symbolmap"

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
    | Function _ -> tinyfail "Should not be locals in AstToWasm.getFunctionSymbols"

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
                tinyfail "TinyFS currently requires a zero parameter 'main' function to exist"
        else
            tinyfail "TinyFS requires a zero parameter 'main' function to exist"
    | Function _ -> tinyfail "Should not be functions"

let astToWasm (decls: FSharpImplementationFileDeclaration list) =
    buildModuleSymbolList decls
    |> confirmMainFunction
    |> getFunctionSymbols
    |> defineFunctionDecls decls
    |> buildModule
