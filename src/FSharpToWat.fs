module rec TinyFS.Core.FSharpToWat

open TinyFS.Core
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text
open System.IO
open System.Collections.Generic
open WatAst
open System
open Transforms

let checker = FSharpChecker.Create(keepAssemblyContents=true)
type ParamTypes = WatType list

let private transformExprList xs: Expr list = List.map (transformExpr []) xs
let private transformExprOpt opt =
    Option.map (transformExpr []) opt

//let mapToList (f: 'a -> 'b) (xs: 'a seq) : 'b list =
//    ([], xs) ||> Seq.fold (fun li x -> (f x) :: li) |> List.rev

let private hashToString (i: int) =
    if i < 0 then
        "Z" + (abs i).ToString("X")
    else
        i.ToString("X")
// From Fable, then https://stackoverflow.com/a/37449594
let private combineHashCodes (hashes: int seq) =
    let hashes = Seq.toArray hashes

    if hashes.Length = 0 then
        0
    else
        hashes |> Array.reduce (fun h1 h2 -> ((h1 <<< 5) + h1) ^^^ h2)
// F# hash function gives different results in different runs
// Taken from Fable OverloadSuffix.fs. Possible variant in https://stackoverflow.com/a/1660613
let private stringHash (s: string) =
    let mutable h = 5381

    for i = 0 to s.Length - 1 do
        h <- (h * 33) ^^^ (int s[i])

    h
let jsKeywords =
    System.Collections.Generic.HashSet
        [
            // Keywords: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Lexical_grammar#Keywords
            "break"
            "case"
            "catch"
            "class"
            "const"
            "continue"
            "debugger"
            "default"
            "delete"
            "do"
            "else"
            "export"
            "extends"
            "finally"
            "for"
            "function"
            "if"
            "import"
            "in"
            "instanceof"
            "new"
            "return"
            "super"
            "switch"
            "this"
            "throw"
            "try"
            "typeof"
            "var"
            "void"
            "while"
            "with"
            "yield"

            "enum"

            "implements"
            "interface"
            "let"
            "package"
            "private"
            "protected"
            "public"
            "static"

            "await"

            "null"
            "true"
            "false"
            "arguments"
            "get"
            "set"

            // Standard built-in objects: https://developer.mozilla.org/en/docs/Web/JavaScript/Reference/Global_Objects
            "Infinity"
            "NaN"
            "undefined"
            "globalThis"

            "eval"
            "uneval"
            "isFinite"
            "isNaN"
            "parseFloat"
            "parseInt"
            "decodeURI"
            "decodeURIComponent"
            "encodeURI"
            "encodeURIComponent"

            "Object"
            "Function"
            "Boolean"
            "Symbol"

            "Error"
            "AggregateError"
            "EvalError"
            "InternalError"
            "RangeError"
            "ReferenceError"
            "SyntaxError"
            "TypeError"
            "URIError"

            "Number"
            "BigInt"
            "Math"
            "Date"

            "String"
            "RegExp"

            "Array"
            "Int8Array"
            "Uint8Array"
            "Uint8ClampedArray"
            "Int16Array"
            "Uint16Array"
            "Int32Array"
            "Uint32Array"
            "Float32Array"
            "Float64Array"
            "BigInt64Array"
            "BigUint64Array"

            "Map"
            "Set"
            "WeakMap"
            "WeakSet"

            "ArrayBuffer"
            "SharedArrayBuffer"
            "Atomics"
            "DataView"
            "JSON"

            "Promise"
            "Generator"
            "GeneratorFunction"
            "AsyncFunction"

            "Reflect"
            "Proxy"

            "Intl"
            "WebAssembly"

            // DOM interfaces (omitting SVG): https://developer.mozilla.org/en-US/docs/Web/API/Document_Object_Model
            "Attr"
            "CDATASection"
            "CharacterData"
            "ChildNode"
            "Comment"
            "CustomEvent"
            "Document"
            "DocumentFragment"
            "DocumentType"
            "DOMError"
            "DOMException"
            "DOMImplementation"
            "DOMString"
            "DOMTimeStamp"
            "DOMStringList"
            "DOMTokenList"
            "Element"
            "Event"
            "EventTarget"
            "HTMLCollection"
            "MutationObserver"
            "MutationRecord"
            "NamedNodeMap"
            "Node"
            "NodeFilter"
            "NodeIterator"
            "NodeList"
            "NonDocumentTypeChildNode"
            "ParentNode"
            "ProcessingInstruction"
            "Selection"
            "Range"
            "Text"
            "TextDecoder"
            "TextEncoder"
            "TimeRanges"
            "TreeWalker"
            "URL"
            "Window"
            "Worker"
            "XMLDocument"

            // Other JS global and special objects/functions. See #258, #1358
            "console"
            "window"
            "document"
            "global"
            "fetch"
        ]
let getNumberFullName kind =
    match kind with
    | Int32 -> Literals.int32
        //function
        //| Int8 -> Types.int8
        //| UInt8 -> Types.uint8
        //| Int16 -> Types.int16
        //| UInt16 -> Types.uint16
        //| Int32 -> Types.int32
        //| UInt32 -> Types.uint32
        //| Int64 -> Types.int64
        //| UInt64 -> Types.uint64
        //| Int128 -> Types.int128
        //| UInt128 -> Types.uint128
        //| NativeInt -> Types.nativeint
        //| UNativeInt -> Types.unativeint
        //| Float16 -> Types.float16
        //| Float32 -> Types.float32
        //| Float64 -> Types.float64
        //| Decimal -> Types.decimal
        //| BigInt -> Types.bigint
let rec private getTypeFastFullName (genParams: IDictionary<_, _>) (t: WatType) =
    match t with
    //| Fable.Measure fullname -> fullname
    //| Fable.GenericParam(name, isMeasure, constraints) ->
    //    if isMeasure then
    //        "measure"
    //    else
    //        match genParams.TryGetValue(name) with
    //        | true, i -> i
    //        | false, _ -> constraints |> List.map (getConstraintHash genParams) |> String.concat ","
    //| Fable.Tuple(genArgs, isStruct) ->
    //    let genArgs =
    //        genArgs |> Seq.map (getTypeFastFullName genParams) |> String.concat " * "

    //    if isStruct then
    //        "struct " + genArgs
    //    else
    //        genArgs
    //| Fable.Array(genArg, kind) ->
    //    let name =
    //        match kind with
    //        | Fable.ResizeArray -> "array"
    //        | Fable.MutableArray -> "resizearray"
    //        | Fable.ImmutableArray -> "immutablearray"

    //    getTypeFastFullName genParams genArg + " " + name
    //| Fable.List genArg -> getTypeFastFullName genParams genArg + " list"
    //| Fable.Option(genArg, isStruct) ->
    //    (if isStruct then
    //         "struct "
    //     else
    //         "")
    //    + (getTypeFastFullName genParams genArg)
    //    + " option"
    //| Fable.LambdaType(argType, returnType) ->
    //    [ argType; returnType ]
    //    |> List.map (getTypeFastFullName genParams)
    //    |> String.concat " -> "
    //// TODO: Use Func` instead?
    //| Fable.DelegateType(argTypes, returnType) ->
    //    argTypes @ [ returnType ]
    //    |> List.map (getTypeFastFullName genParams)
    //    |> String.concat " -> "
    //| Fable.AnonymousRecordType(fieldNames, genArgs, isStruct) ->
    //    let fields =
    //        Seq.zip fieldNames genArgs
    //        |> Seq.map (fun (key, typ) -> key + " : " + getTypeFastFullName genParams typ)
    //        |> String.concat "; "

    //    (if isStruct then
    //         "struct "
    //     else
    //         "")
    //    + "{|"
    //    + fields
    //    + "|}"
    //| Fable.DeclaredType(tdef, genArgs) ->
    //    let genArgs = genArgs |> Seq.mapToList (getTypeFastFullName genParams)
    //    // Not sure why, but when precompiling F# changes measure types to MeasureProduct<'M, MeasureOne>
    //    match tdef.FullName, genArgs with
    //    | Types.measureProduct2, [ measure; Types.measureOne ] ->
    //        // TODO: generalize it to support aggregate units such as <m/s> or more complex
    //        measure
    //    | _ ->
    //        let genArgs = String.concat "," genArgs

    //        let genArgs =
    //            if genArgs = "" then
    //                ""
    //            else
    //                "[" + genArgs + "]"

    //        tdef.FullName + genArgs
    //| Fable.MetaType -> Types.type_
    //| Fable.Any -> Types.object
    //| Fable.Unit -> Types.unit
    //| Fable.Boolean -> Types.bool
    //| Fable.Char -> Types.char
    //| Fable.String -> Types.string
    //| Fable.Regex -> Types.regex
    | WatType.Number(kind) -> getNumberFullName kind

let parseAndCheckSingleFile (input: string) = 
    let file = Path.ChangeExtension(System.IO.Path.GetTempFileName(), "fsx")  
    File.WriteAllText(file, input)
    // Get context representing a stand-alone (script) file
    let projOptions, _errors = 
        checker.GetProjectOptionsFromScript(file, SourceText.ofString input, assumeDotNetFramework=false)
        |> Async.RunSynchronously

    checker.ParseAndCheckProject(projOptions) 
    |> Async.RunSynchronously
let private getHashPrivate (paramTypes: ParamTypes) genParams =
    paramTypes
    |> List.map (getTypeFastFullName genParams >> stringHash)
    |> combineHashCodes
    |> hashToString

let hasEmptyOverloadSuffix (curriedParamTypes: ParamTypes) =
    // Don't use overload suffix for members without arguments
    match curriedParamTypes with
    | [] -> true
    | [ WatType.Unit ] -> true
    | _ -> false

let getHash (entityGenericParams: string list) (curriedParamTypeGroups: WatType list list) =
    match curriedParamTypeGroups with
    | [ paramTypes ] ->
        if hasEmptyOverloadSuffix paramTypes then
            ""
        else
            // Generics can have different names in signature
            // and implementation files, use the position instead
            let genParams =
                entityGenericParams |> List.mapi (fun i p -> p, string<int> i) |> dict

            getHashPrivate paramTypes genParams
    // Members with curried params cannot be overloaded in F#
    // TODO: Also private methods defined with `let` cannot be overloaded
    // but I don't know how to identify them in the AST
    | _ -> ""

/// Used for extension members
let getExtensionHash (curriedParamTypeGroups: WatType list list) =
    match curriedParamTypeGroups with
    | [ paramTypes ] ->
        if hasEmptyOverloadSuffix paramTypes then
            ""
        else
            // Type resolution in extension member seems to be different
            // and doesn't take generics into account
            dict [] |> getHashPrivate paramTypes
    // Members with curried params cannot be overloaded in F#
    | _ -> ""

let isAttachMembersEntity (ent: FSharpEntity) =
    not (ent.IsFSharpModule || ent.IsInterface)
    && (
    ent.Attributes
    |> Seq.exists (fun att ->
        // Should we make sure the attribute is not an alias?
        match att.AttributeType.TryFullName with
        | Some "" -> true
        | _ -> false
    ))

let private transformExplicitlyAttachedMember
    (declaringEntity: FSharpEntity)
    (memb: FSharpMemberOrFunctionOrValue)
    args
    (body: FSharpExpr) : WatAst.Declaration list
    =
    []
    //let bodyCtx, args = bindMemberArgs com ctx args
    //let body = transformExpr com bodyCtx [] body |> run
    //let entFullName = declaringEntity.FullName

    //let name, isMangled =
    //    match Compiler.Language with
    //    | Rust -> getMemberDeclarationName com memb |> fst, true
    //    | _ -> FsMemberFunctionOrValue.CompiledName(memb), false

    //com.AddAttachedMember(
    //    entFullName,
    //    isMangled = false,
    //    memb =
    //        {
    //            Name = name
    //            Body = body
    //            Args = args
    //            IsMangled = isMangled
    //            UsedNames = set ctx.UsedNamesInDeclarationScope
    //            MemberRef = getFunctionMemberRef memb
    //            ImplementedSignatureRef = None
    //            Tags = Fable.Tags.empty
    //            XmlDoc = tryGetXmlDoc memb.XmlDoc
    //        }
    //)





let hasAttrib attFullName (attributes: FSharpAttribute seq) =
    attributes
    |> Seq.exists (fun att ->
        match (Transforms.nonAbbreviatedDefinition att.AttributeType).TryFullName with
        | Some attFullName2 -> attFullName = attFullName2
        | None -> false
    )
let hasParamArray (memb: FSharpMemberOrFunctionOrValue) =

    let hasParamArray (memb: FSharpMemberOrFunctionOrValue) =
        if memb.CurriedParameterGroups.Count <> 1 then
            false
        else
            let args = memb.CurriedParameterGroups[0]
            args.Count > 0 && args[args.Count - 1].IsParamArrayArg

    let hasParamSeq (memb: FSharpMemberOrFunctionOrValue) =
        Seq.tryLast memb.CurriedParameterGroups
        |> Option.bind Seq.tryLast
        //|> Option.map (fun lastParam -> hasAttrib Atts.paramList lastParam.Attributes)
        |> Option.map (fun _ -> false)
        |> Option.defaultValue false

    hasParamArray memb || hasParamSeq memb
let getFunctionMemberRef (memb: FSharpMemberOrFunctionOrValue) =
    match memb.DeclaringEntity with
    // We cannot retrieve compiler generated members from the entity
    | Some ent when not memb.IsCompilerGenerated ->
        let nonCurriedArgTypes =
            if memb.CurriedParameterGroups.Count = 1 then
                memb.CurriedParameterGroups[0]
                |> Helpers.Seq.mapToList (fun p -> makeType p.Type)
                |> Some
            else
                None

        let fableMemberFunctionOrValue =
            FsMemberFunctionOrValue(memb) :> WatAst.MemberFunctionOrValue

        let attributeFullNames =
            fableMemberFunctionOrValue.Attributes
            |> Seq.map (fun attr -> attr.Entity.FullName)
            |> List.ofSeq

        WatAst.MemberRef(
            Ref(ent),
            {
                CompiledName = memb.CompiledName
                IsInstance = memb.IsInstanceMember
                NonCurriedArgTypes = nonCurriedArgTypes
                AttributeFullNames = attributeFullNames
            }
        )
    | ent ->
        let entRef = ent |> Option.map Ref

        let argTypes =
            memb.CurriedParameterGroups
            |> Seq.concat
            |> Helpers.Seq.mapToList (fun p -> makeType p.Type)

        let returnType = makeType memb.ReturnParameter.Type

        GeneratedMember.Function(
            memb.CompiledName,
            argTypes,
            returnType,
            isInstance = memb.IsInstanceMember,
            hasSpread = hasParamArray memb,
            ?entRef = entRef
        )
let transformOptionalArguments
    (memb: FSharpMemberOrFunctionOrValue)
    (args: WatAst.Expr list)
    =
    if
        memb.CurriedParameterGroups.Count <> 1
        || memb.CurriedParameterGroups[0].Count <> (List.length args)
    then
        args
    else
        (memb.CurriedParameterGroups[0], args, (true, []))
        |||> Seq.foldBack2 (fun par arg (keepChecking, acc) ->
            //if keepChecking && par.IsOptionalArg then
            //    match arg with
            //    | WatAst.Value(Fable.NewOption(None, _, _), _) -> true, acc
            //    | _ -> false, arg :: acc
            //else
                false, arg :: acc
        )
        |> snd
let getArgTypes (memb: FSharpMemberOrFunctionOrValue) =
    // FSharpParameters don't contain the `this` arg
    Seq.concat memb.CurriedParameterGroups
    // The F# compiler "untuples" the args in methods
    |> Seq.map (fun x -> makeType x.Type)
    |> Seq.toList
let (|Try|_|) (f: 'a -> 'b option) a = f a

//Not using context, so this can't be used
//let tryGetIdentFromScope typ (fsRef: FSharpMemberOrFunctionOrValue) =
//    tryGetIdentFromScopeIf r typ fsRef.Equals

let private isReplacementCandidatePrivate isFromDll (entFullName: string) =
    if
        entFullName.StartsWith("System.", StringComparison.Ordinal)
        || entFullName.StartsWith("Microsoft.FSharp.", StringComparison.Ordinal)
    then
        isFromDll ()
    // When compiling Fable itself, Fable.Core entities will be part of the code base, but still need to be replaced
    else
        entFullName.StartsWith("Fable.Core.", StringComparison.Ordinal)
        && (not (entFullName.StartsWith("Fable.Core.JS.", StringComparison.Ordinal))
            || entFullName.EndsWith("Attribute", StringComparison.Ordinal))
let isReplacementCandidateFrom (ent: FSharpEntity) =
    let isFromDll () = Option.isSome ent.Assembly.FileName
    isReplacementCandidatePrivate isFromDll (FullName ent)
// Mutable public values must be called as functions in JS (see #986)
let isModuleValueCompiledAsFunction (memb: FSharpMemberOrFunctionOrValue) =
    memb.IsMutable && isNotPrivate memb

let isModuleValueForCalls (declaringEntity: FSharpEntity) (memb: FSharpMemberOrFunctionOrValue) =
    declaringEntity.IsFSharpModule
    && Helpers.isModuleValueForDeclarations memb
    && not (isModuleValueCompiledAsFunction memb)

let genParamName (genParam: FSharpGenericParameter) =
    // Sometimes the names of user-declared and compiler-generated clash, see #1900 and https://github.com/dotnet/fsharp/issues/13062
    let name = genParam.Name.Replace("?", "$")

    if genParam.IsCompilerGenerated then
        "$" + name
    else
        name

let getOverloadSuffixFrom (ent: FSharpEntity) (memb: FSharpMemberOrFunctionOrValue) =
    match ent.CompiledName with
    // HACK for compiling FSharpMap/FSharpSet in fable-library
    | "FSharpMap"
    | "FSharpSet" -> ""
    | _ ->
        let entGenParams = ent.GenericParameters |> Helpers.Seq.mapToList genParamName

        memb.CurriedParameterGroups
        |> Helpers.Seq.mapToList (Helpers.Seq.mapToList (fun p -> makeType p.Type))
        |> getHash entGenParams

let toInt targetType (args: Expr list) =
    let arg = args.Head
    // TODO: Review this and include Int64
    let emitCast typeTo arg = arg
    //        match typeTo with
    //        | Int8 -> emitExpr None Int8.Number [arg] "($0 + 0x80 & 0xFF) - 0x80"
    //        | Int16 -> emitExpr None Int16.Number [arg] "($0 + 0x8000 & 0xFFFF) - 0x8000"
    //        | Int32 -> fastIntFloor arg
    //        | UInt8 -> emitExpr None UInt8.Number [arg] "$0 & 0xFF"
    //        | UInt16 -> emitExpr None UInt16.Number [arg] "$0 & 0xFFFF"
    //        | UInt32 -> emitExpr None UInt32.Number [arg] "$0 >>> 0"
    //        | _ -> FableError $"Unexpected non-integer type %A{typeTo}" |> raise
    match arg.Type, targetType with
    | Char, Number(typeTo) -> emitCast typeTo arg
    //| String, _ -> stringToInt com ctx r targetType args
    //| Number(BigInt, _), _ -> Helper.LibCall(com, "BigInt", castBigIntMethod targetType, targetType, args)
    //| Number(typeFrom, _), Number(typeTo, _) ->
    //    if needToCast typeFrom typeTo then
    //        match typeFrom with
    //        | Decimal -> Helper.LibCall(com, "Decimal", "toNumber", targetType, args) |> emitCast typeTo
    //        | DartInt -> arg |> emitCast typeTo
    //        | _ -> Helper.InstanceCall(arg, "toInt", targetType, [])
    //    else
    //        TypeCast(arg, targetType)
    | _ ->
        failwith "Cannot make conversion because source type is unknown"
let applyOp t opName (args: Expr list) =
    let unOp operator operand =
        WatAst.Operation(Unary(operator, operand), [], t)

    let binOp op left right =
        WatAst.Operation(Binary(op, left, right), [], t)

    //let binOpChar op left right =
    //    let toUInt16 e =
    //        toInt None (Number(UInt16, NumberInfo.Empty)) [ e ]

    //    WatAst.Operation(Binary(op, toUInt16 left, toUInt16 right), Tags.empty, UInt16.Number, r)
    //    |> toChar

    //let truncateUnsigned operation = // see #1550
    //    match t with
    //    | Number(UInt32, _) -> Operation(Binary(BinaryShiftRightZeroFill, operation, makeIntConst 0), Tags.empty, t, r)
    //    | _ -> operation

    //let logicOp op left right =
    //    Operation(Logical(op, left, right), Tags.empty, Boolean, r)

    let nativeOp opName argTypes args =
        match opName, args with
        | Operators.addition, [ left; right ] ->
            match argTypes with
            //| Char :: _ -> binOpChar BinaryPlus left right
            | _ -> binOp BinaryPlus left right
        | Operators.subtraction, [ left; right ] ->
            match argTypes with
            //| Char :: _ -> binOpChar BinaryMinus left right
            | _ -> binOp BinaryMinus left right
        | Operators.multiply, [ left; right ] -> binOp BinaryMultiply left right
        | (Operators.division | Operators.divideByInt), [ left; right ] -> binOp BinaryDivide left right
        //// In dart % operator and .remainder give different values for negative numbers
        //| Operators.modulus, [ left; right ] -> InstanceCall(left, "remainder", t, [ right ])
        //| Operators.leftShift, [ left; right ] -> binOp BinaryShiftLeft left right |> truncateUnsigned // See #1530
        //| Operators.rightShift, [ left; right ] ->
        //    match argTypes with
        //    | Number(UInt32, _) :: _ -> binOp BinaryShiftRightZeroFill left right // See #646
        //    | _ -> binOp BinaryShiftRightSignPropagating left right
        //| Operators.bitwiseAnd, [ left; right ] -> binOp BinaryAndBitwise left right |> truncateUnsigned
        //| Operators.bitwiseOr, [ left; right ] -> binOp BinaryOrBitwise left right |> truncateUnsigned
        //| Operators.exclusiveOr, [ left; right ] -> binOp BinaryXorBitwise left right |> truncateUnsigned
        //| Operators.booleanAnd, [ left; right ] -> logicOp LogicalAnd left right
        //| Operators.booleanOr, [ left; right ] -> logicOp LogicalOr left right
        //| Operators.logicalNot, [ operand ] -> unOp UnaryNotBitwise operand |> truncateUnsigned
        //| Operators.unaryNegation, [ operand ] ->
        //    // TODO: Check for min value, see "Unary negation with integer MinValue works" test
        //    unOp UnaryMinus operand
        // match argTypes with
        // | Number(Int8,_)::_ -> Helper.LibCall(com, "Int32", "op_UnaryNegation_Int8", t, args, ?loc=r)
        // | Number(Int16,_)::_ -> Helper.LibCall(com, "Int32", "op_UnaryNegation_Int16", t, args, ?loc=r)
        // | Number(Int32,_)::_ -> Helper.LibCall(com, "Int32", "op_UnaryNegation_Int32", t, args, ?loc=r)
        // | _ -> unOp UnaryMinus operand
        //| Operators.unaryPlus, [ operand ] -> unOp UnaryPlus operand
        | _ ->
            failwith $"Operator %s{opName} not found in %A{argTypes}"
            //|> addErrorAndReturnNull com ctx.InlinePath r

    let argTypes = args |> List.map (fun a -> a.Type)

    //match argTypes with
    //| Number(BigInt | Decimal as kind, _) :: _ ->
    //    let modName, opName =
    //        match kind, opName with
    //        | UInt64, Operators.rightShift -> "Long", "op_RightShiftUnsigned" // See #1482
    //        | Decimal, Operators.divideByInt -> "Decimal", Operators.division
    //        | Decimal, _ -> "Decimal", opName
    //        //            | BigInt, _ -> "BigInt", opName
    //        | _ -> "BigInt", opName

    //    Helper.LibCall(com, modName, opName, t, args, argTypes, ?loc = r)
    //| Builtin(BclDateTime | BclTimeSpan | BclDateTimeOffset | BclDateOnly as bt) :: _ ->
    //    let meth =
    //        match opName with
    //        | "op_Addition" -> "add"
    //        | "op_Subtraction" -> getSubtractToDateMethodName args
    //        | "op_Multiply" -> "multiply"
    //        | "op_Division" -> "divide"
    //        | _ -> opName

    //    Helper.LibCall(com, coreModFor bt, meth, t, args, argTypes, ?loc = r)
    //| Builtin(FSharpSet _) :: _ ->
    //    let mangledName = Naming.buildNameWithoutSanitationFrom "FSharpSet" true opName ""

    //    Helper.LibCall(com, "Set", mangledName, t, args, argTypes, ?loc = r)
    //// | Builtin (FSharpMap _)::_ ->
    ////     let mangledName = Naming.buildNameWithoutSanitationFrom "FSharpMap" true opName overloadSuffix.Value
    ////     Helper.LibCall(com, "Map", mangledName, t, args, argTypes, ?loc=r)
    //| CustomOp com ctx r t opName args e -> e
    //| _ -> 
    nativeOp opName argTypes args

let operators t (i: ReplaceCallInfo) (thisArg: Expr option) (args: Expr list) =
    let math t (args: Expr list) argTypes methName =
        let meth = Naming.lowerFirst methName
        Helper.GlobalCall("Math", t, args, argTypes, memb = meth)

    match i.CompiledName, args with
    //| ("DefaultArg" | "DefaultValueArg"), [ opt; defValue ] ->
    //    match opt with
    //    | MaybeInScope ctx (Value(NewOption(opt, _, _), _)) ->
    //        match opt with
    //        | Some value -> Some value
    //        | None -> Some defValue
    //    | _ ->
    //        Helper.LibCall(com, "Option", "defaultArg", t, args, i.SignatureArgTypes, genArgs = i.GenericArgs, ?loc = r)
    //        |> Some
    //| "DefaultAsyncBuilder", _ -> makeImportLib com t "singleton" "AsyncBuilder" |> Some
    //// Erased operators.
    //// KeyValuePair is already compiled as a tuple
    //| ("KeyValuePattern" | "Identity" | "Box" | "Unbox" | "ToEnum"), [ arg ] -> TypeCast(arg, t) |> Some
    //// Cast to unit to make sure nothing is returned when wrapped in a lambda, see #1360
    //| "Ignore", _ -> TypeCast(args.Head, Unit) |> Some
    //// Number and String conversions
    //| ("ToSByte" | "ToByte" | "ToInt8" | "ToUInt8" | "ToInt16" | "ToUInt16" | "ToInt" | "ToUInt" | "ToInt32" | "ToUInt32"),
    //  _ -> toInt com ctx r t args |> Some
    //| ("ToInt64" | "ToUInt64" | "ToIntPtr" | "ToUIntPtr"), _ -> toLong com ctx r t args |> Some
    //| ("ToSingle" | "ToDouble"), _ -> toFloat com ctx r t args |> Some
    //| "ToDecimal", _ -> toDecimal com ctx r t args |> Some
    //| "ToChar", _ -> toChar args.Head |> Some
    //| "ToString", _ -> toString com ctx r args |> Some
    //| "CreateSequence", [ xs ] -> TypeCast(xs, t) |> Some
    //| ("CreateDictionary" | "CreateReadOnlyDictionary"), [ arg ] -> makeDictionary com ctx r t arg |> Some
    //| "CreateSet", _ -> (genArg com ctx r 0 i.GenericArgs) |> makeSet com ctx r t "OfSeq" args |> Some
    //// Ranges
    //| ("op_Range" | "op_RangeStep"), _ ->
    //    let genArg = genArg com ctx r 0 i.GenericArgs

    //    let addStep args =
    //        match args with
    //        | [ first; last ] -> [ first; getOne com ctx genArg; last ]
    //        | _ -> args

    //    let modul, meth, args =
    //        match genArg with
    //        | Char -> "Range", "rangeChar", args
    //        | Number(Int64, _) -> "Range", "rangeInt64", addStep args
    //        | Number(UInt64, _) -> "Range", "rangeUInt64", addStep args
    //        | Number(Decimal, _) -> "Range", "rangeDecimal", addStep args
    //        | Number(Numbers _, _) -> "Range", "rangeDouble", addStep args
    //        | Number(BigIntegers _, _) -> "Range", "rangeBigInt", addStep args
    //        | x -> FableError "Unsupported range type: %A{x}" |> raise

    //    Helper.LibCall(com, modul, meth, t, args, i.SignatureArgTypes, ?loc = r) |> Some
    //// Pipes and composition
    //| "op_PipeRight", [ x; f ]
    //| "op_PipeLeft", [ f; x ] -> curriedApply r t f [ x ] |> Some
    //| "op_PipeRight2", [ x; y; f ]
    //| "op_PipeLeft2", [ f; x; y ] -> curriedApply r t f [ x; y ] |> Some
    //| "op_PipeRight3", [ x; y; z; f ]
    //| "op_PipeLeft3", [ f; x; y; z ] -> curriedApply r t f [ x; y; z ] |> Some
    //| "op_ComposeRight", [ f1; f2 ] -> compose com ctx r t f1 f2 |> Some
    //| "op_ComposeLeft", [ f2; f1 ] -> compose com ctx r t f1 f2 |> Some
    //// Strings
    //| ("PrintFormatToString" | "PrintFormatToStringThen" | "PrintFormat" | "PrintFormatLine" | "PrintFormatToError" | "PrintFormatLineToError" | "PrintFormatThen" | "PrintFormatToStringThenFail" | "PrintFormatToStringBuilder" | "PrintFormatToStringBuilderThen"), // Printf.kbprintf
    //  _ -> fsFormat com ctx r t i thisArg args
    //| ("Failure" | "FailurePattern" | "LazyPattern" | "Lock" | "NullArg" | "Using"), _ ->
    //    fsharpModule com ctx r t i thisArg args
    //| ("IsNull" | "IsNotNull" | "IsNullV" | "NonNull" | "NonNullV" | "NullMatchPattern" | "NullValueMatchPattern" | "NonNullQuickPattern" | "NonNullQuickValuePattern" | "WithNull" | "WithNullV" | "NullV" | "NullArgCheck"),
    //  _ -> fsharpModule com ctx r t i thisArg args
    //// Exceptions
    //| "FailWith", [ msg ]
    //| "InvalidOp", [ msg ] -> makeThrow r t (error msg) |> Some
    //| "InvalidArg", [ argName; msg ] ->
    //    let msg = add (add msg (str "\\nParameter name: ")) argName
    //    makeThrow r t (error msg) |> Some
    //| "Raise", [ arg ] -> makeThrow r t arg |> Some
    //| "Reraise", _ ->
    //    match ctx.CaughtException with
    //    | Some ex -> makeThrow r t (IdentExpr ex) |> Some
    //    | None ->
    //        "`reraise` used in context where caught exception is not available, please report"
    //        |> addError com ctx.InlinePath r

    //        makeThrow r t (error (str "")) |> Some
    //// Math functions
    //// TODO: optimize square pow: x * x
    //| "Pow", _
    //| "PowInteger", _
    //| "op_Exponentiation", _ ->
    //    let argTypes = args |> List.map (fun a -> a.Type)

    //    match argTypes with
    //    | Number(Decimal, _) :: _ ->
    //        Helper.LibCall(com, "Decimal", "pow", t, args, i.SignatureArgTypes, ?thisArg = thisArg, ?loc = r)
    //        |> Some
    //    | CustomOp com ctx r t "Pow" args e -> Some e
    //    | _ -> math r t args i.SignatureArgTypes "pow" |> Some
    //| ("Ceiling" | "Floor" as meth), _ ->
    //    let meth = Naming.lowerFirst meth

    //    match args with
    //    | ExprType(Number(Decimal, _)) :: _ ->
    //        Helper.LibCall(com, "Decimal", meth, t, args, i.SignatureArgTypes, ?thisArg = thisArg, ?loc = r)
    //        |> Some
    //    | _ ->
    //        let meth =
    //            if meth = "ceiling" then
    //                "ceil"
    //            else
    //                meth

    //        math r t args i.SignatureArgTypes meth |> Some
    //| "Log", [ arg1; arg2 ] ->
    //    // "Math.log($0) / Math.log($1)"
    //    let dividend = math None t [ arg1 ] (List.take 1 i.SignatureArgTypes) "log"

    //    let divisor = math None t [ arg2 ] (List.skip 1 i.SignatureArgTypes) "log"

    //    makeBinOp r t dividend divisor BinaryDivide |> Some
    //| "Abs", _ ->
    //    match args with
    //    | ExprType(Number(Decimal, _)) :: _ ->
    //        Helper.LibCall(com, "Decimal", "abs", t, args, i.SignatureArgTypes, ?thisArg = thisArg, ?loc = r)
    //        |> Some
    //    | ExprType(Number(BigIntegers _, _)) :: _ ->
    //        Helper.LibCall(com, "BigInt", "abs", t, args, i.SignatureArgTypes, ?thisArg = thisArg, ?loc = r)
    //        |> Some
    //    | _ -> math r t args i.SignatureArgTypes i.CompiledName |> Some
    //| ("Acos" | "Asin" | "Atan" | "Atan2" | "Cos" | "Cosh" | "Exp" | "Log" | "Log2" | "Log10" | "Sin" | "Sinh" | "Sqrt" | "Tan" | "Tanh"),
    //  _ ->
    //    match args with
    //    | ExprType(Number(_, _)) :: _ -> math r t args i.SignatureArgTypes i.CompiledName |> Some
    //    | _ -> applyOp com ctx r t i.CompiledName args |> Some
    //| "Round", _ ->
    //    match args with
    //    | ExprType(Number(Decimal, _)) :: _ ->
    //        Helper.LibCall(com, "Decimal", "round", t, args, i.SignatureArgTypes, ?thisArg = thisArg, ?loc = r)
    //        |> Some
    //    | _ ->
    //        Helper.LibCall(com, "Util", "round", t, args, i.SignatureArgTypes, ?thisArg = thisArg, ?loc = r)
    //        |> Some
    //| "Truncate", _ ->
    //    match args with
    //    | ExprType(Number(Decimal, _)) :: _ ->
    //        Helper.LibCall(com, "Decimal", "truncate", t, args, i.SignatureArgTypes, ?thisArg = thisArg, ?loc = r)
    //        |> Some
    //    | _ ->
    //        Helper.GlobalCall("Math", t, args, i.SignatureArgTypes, memb = "trunc", ?loc = r)
    //        |> Some
    //| "Sign", _ ->
    //    let args = toFloat com ctx r t args |> List.singleton

    //    Helper.LibCall(com, "Util", "sign", t, args, i.SignatureArgTypes, ?loc = r)
    //    |> Some
    //| "DivRem", _ ->
    //    match i.SignatureArgTypes with
    //    | Number(BigIntegers _, _) :: _ ->
    //        Helper.LibCall(com, "BigInt", "divRem", t, args, i.SignatureArgTypes, ?loc = r)
    //        |> Some
    //    | _ ->
    //        Helper.LibCall(com, "Int32", "divRem", t, args, i.SignatureArgTypes, ?loc = r)
    //        |> Some
    //// Numbers
    //| ("Infinity" | "InfinitySingle"), _ -> Helper.GlobalIdent("Number", "POSITIVE_INFINITY", t, ?loc = r) |> Some
    //| ("NaN" | "NaNSingle"), _ -> Helper.GlobalIdent("Number", "NaN", t, ?loc = r) |> Some
    //| "Fst", [ tup ] -> Get(tup, TupleIndex 0, t, r) |> Some
    //| "Snd", [ tup ] -> Get(tup, TupleIndex 1, t, r) |> Some
    //// Reference
    //| "op_Dereference", [ arg ] -> getRefCell com r t arg |> Some
    //| "op_ColonEquals", [ o; v ] -> setRefCell com r o v |> Some
    //| "Ref", [ arg ] -> makeRefCellFromValue com r arg |> Some
    //| ("Increment" | "Decrement"), _ ->
    //    if i.CompiledName = "Increment" then
    //        "void($0.contents++)"
    //    else
    //        "void($0.contents--)"
    //    |> emitExpr r t args
    //    |> Some
    //// Concatenates two lists
    //| "op_Append", _ ->
    //    Helper.LibCall(com, "List", "append", t, args, i.SignatureArgTypes, ?thisArg = thisArg, ?loc = r)
    //    |> Some
    //| (Operators.inequality | "Neq"), [ left; right ] -> equals com ctx r false left right |> Some
    //| (Operators.equality | "Eq"), [ left; right ] -> equals com ctx r true left right |> Some
    //| "IsNull", [ arg ] -> nullCheck r true arg |> Some
    //| "Hash", [ arg ] -> structuralHash com r arg |> Some
    //// Comparison
    //| "Compare", [ left; right ] -> compare com ctx r left right |> Some
    //| (Operators.lessThan | "Lt"), [ left; right ] -> booleanCompare com ctx r left right BinaryLess |> Some
    //| (Operators.lessThanOrEqual | "Lte"), [ left; right ] ->
    //    booleanCompare com ctx r left right BinaryLessOrEqual |> Some
    //| (Operators.greaterThan | "Gt"), [ left; right ] -> booleanCompare com ctx r left right BinaryGreater |> Some
    //| (Operators.greaterThanOrEqual | "Gte"), [ left; right ] ->
    //    booleanCompare com ctx r left right BinaryGreaterOrEqual |> Some
    //| ("Min" | "Max" | "MinMagnitude" | "MaxMagnitude" | "Clamp" as meth), _ ->
    //    let meth = Naming.lowerFirst meth

    //    match args with
    //    | ExprType(Number(Decimal, _)) :: _ ->
    //        Helper.LibCall(com, "Decimal", meth, t, args, i.SignatureArgTypes, ?thisArg = thisArg, ?loc = r)
    //        |> Some
    //    | ExprType(Number(BigIntegers _, _)) :: _ ->
    //        Helper.LibCall(com, "BigInt", meth, t, args, i.SignatureArgTypes, ?thisArg = thisArg, ?loc = r)
    //        |> Some
    //    | ExprType(Number _) :: _ ->
    //        Helper.LibCall(com, "Double", meth, t, args, i.SignatureArgTypes, ?thisArg = thisArg, ?loc = r)
    //        |> Some
    //    | _ ->
    //        let f = makeComparerFunction com ctx t

    //        Helper.LibCall(com, "Util", meth, t, f :: args, i.SignatureArgTypes, genArgs = i.GenericArgs, ?loc = r)
    //        |> Some
    //| "Not", [ operand ] -> // TODO: Check custom operator?
    //    makeUnOp r t operand UnaryNot |> Some
    | Patterns.SetContains TinyFS.Core.Transforms.Operators.standardSet, _ -> applyOp t i.CompiledName args |> Some
    // Type info
    //| "TypeOf", _ ->
    //    (genArg com ctx r 0 i.GenericArgs)
    //    |> makeTypeInfo (changeRangeToCallSite ctx.InlinePath r)
    //    |> Some
    //| "TypeDefOf", _ ->
    //    (genArg com ctx r 0 i.GenericArgs)
    //    |> makeTypeDefinitionInfo (changeRangeToCallSite ctx.InlinePath r)
    //    |> Some
    | _ -> None
let private replacedModules =
    dict
        [
            "System.Math", operators
            "System.MathF", operators
            "Microsoft.FSharp.Core.Operators", operators
            "Microsoft.FSharp.Core.Operators.Checked", operators
        ]

let tryReplace t (info: ReplaceCallInfo) (thisArg: Expr option) (args: Expr list) =
    match info.DeclaringEntityFullName with
    | Patterns.DicContains replacedModules replacement -> replacement t info thisArg args
    //| "Microsoft.FSharp.Core.LanguagePrimitives.ErrorStrings" -> errorStrings info.CompiledName
    //| Types.printfModule
    //| Naming.StartsWith Types.printfFormat _ -> fsFormat com ctx r t info thisArg args
    //| Naming.StartsWith "Fable.Core." _ -> fableCoreLib com ctx r t info thisArg args
    //| Naming.EndsWith "Exception" _ -> exceptions com ctx r t info thisArg args
    //| "System.Timers.ElapsedEventArgs" -> thisArg // only signalTime is available here
    //| Naming.StartsWith "System.Tuple" _
    //| Naming.StartsWith "System.ValueTuple" _ -> tuples com ctx r t info thisArg args
    //| "System.Delegate"
    //| Naming.StartsWith "System.Action" _
    //| Naming.StartsWith "System.Func" _
    //| Naming.StartsWith "Microsoft.FSharp.Core.FSharpFunc" _
    //| Naming.StartsWith "Microsoft.FSharp.Core.OptimizedClosures.FSharpFunc" _ -> funcs com ctx r t info thisArg args
    //| "Microsoft.FSharp.Reflection.FSharpType" -> fsharpType com info.CompiledName r t info args
    //| "Microsoft.FSharp.Reflection.FSharpValue" -> fsharpValue com info.CompiledName r t info args
    //| "Microsoft.FSharp.Reflection.FSharpReflectionExtensions" ->
    //    // In netcore F# Reflection methods become extensions
    //    // with names like `FSharpType.GetExceptionFields.Static`
    //    let isFSharpType =
    //        info.CompiledName.StartsWith("FSharpType", StringComparison.Ordinal)

    //    let methName = info.CompiledName |> Naming.extensionMethodName

    //    if isFSharpType then
    //        fsharpType com methName r t info args
    //    else
    //        fsharpValue com methName r t info args
    //| "Microsoft.FSharp.Reflection.UnionCaseInfo"
    //| "System.Reflection.PropertyInfo"
    //| "System.Reflection.ParameterInfo"
    //| "System.Reflection.MethodBase"
    //| "System.Reflection.MethodInfo"
    //| "System.Reflection.MemberInfo" ->
    //    match thisArg, info.CompiledName with
    //    | Some c, "get_Tag" -> makeStrConst "tag" |> getExpr r t c |> Some
    //    | Some c, "get_ReturnType" -> makeStrConst "returnType" |> getExpr r t c |> Some
    //    | Some c, "GetParameters" -> makeStrConst "parameters" |> getExpr r t c |> Some
    //    | Some c, ("get_PropertyType" | "get_ParameterType") -> makeIntConst 1 |> getExpr r t c |> Some
    //    | Some c, "GetFields" ->
    //        Helper.LibCall(com, "Reflection", "getUnionCaseFields", t, [ c ], ?loc = r)
    //        |> Some
    //    | Some c, "GetValue" -> Helper.LibCall(com, "Reflection", "getValue", t, c :: args, ?loc = r) |> Some
    //    | Some c, "get_Name" ->
    //        match c with
    //        | Value(TypeInfo(exprType, _), loc) ->
    //            getTypeName com ctx loc exprType |> StringConstant |> makeValue r |> Some
    //        | c -> Helper.LibCall(com, "Reflection", "name", t, [ c ], ?loc = r) |> Some
    //    | _ -> None
    | _ -> None
let (|Replaced|_|)
    (typ: WatType)
    (callInfo: WatAst.CallInfo)
    (memb: FSharpMemberOrFunctionOrValue, entity: FSharpEntity option)
    =
    match entity with
    | Some ent when isReplacementCandidateFrom ent ->
        let info: WatAst.ReplaceCallInfo =
            {
                SignatureArgTypes = callInfo.SignatureArgTypes
                DeclaringEntityFullName = ent.FullName
                HasSpread = hasParamArray memb
                IsModuleValue = isModuleValueForCalls ent memb
                IsInterface = ent.IsInterface
                CompiledName = memb.CompiledName
                OverloadSuffix =
                    if ent.IsFSharpModule then
                        ""
                    else
                        getOverloadSuffixFrom ent memb
                GenericArgs = callInfo.GenericArgs
            }

        //match ctx.PrecompilingInlineFunction with
        //| Some _ ->
        //    // Deal with reraise so we don't need to save caught exception every time
        //    match ctx.CaughtException, info.DeclaringEntityFullName, info.CompiledName with
        //    | Some ex, "Microsoft.FSharp.Core.Operators", "Reraise" when com.Options.Language <> Dart ->
        //        makeThrow r typ (Fable.IdentExpr ex) |> Some
        //    | _ ->
        //        // If it's an interface compile the call to the attached member just in case
        //        let attachedCall =
        //            if info.IsInterface then
        //                callAttachedMember com ctx r typ callInfo ent memb |> Some
        //            else
        //                None

        //        let e =
        //            Fable.UnresolvedReplaceCall(callInfo.ThisArg, callInfo.Args, info, attachedCall)

        //        Fable.Unresolved(e, typ, r) |> Some
        //| None ->
        let replacementTry = tryReplace typ info callInfo.ThisArg callInfo.Args
        match replacementTry with
        | Some e -> Some e
        //| None when info.IsInterface -> callAttachedMember com ctx r typ callInfo ent memb |> Some
        //| None -> failReplace com ctx r info callInfo.ThisArg |> Some
        | _ -> failwith "Cannot replace for some reason"
    | _ -> None
let makeCallWithArgInfo
    typ
    callee
    (memb: FSharpMemberOrFunctionOrValue)
    membRef
    (callInfo: WatAst.CallInfo)
    =
    match memb, memb.DeclaringEntity with
    //| Emitted com ctx r typ (Some callInfo) emitted, _ -> emitted
    //| Imported com ctx r typ (Some callInfo) imported -> imported
    | Replaced typ callInfo replaced -> replaced
    //| Inlined com ctx r typ callee callInfo expr, _ -> expr

    //| Try (tryGetIdentFromScope ctx r None) funcExpr, Some entity ->
    //    if isModuleValueForCalls com entity memb then
    //        funcExpr
    //    else
    //        makeCall r typ callInfo funcExpr

    //| _, Some entity when entity.IsDelegate ->
    //    match callInfo.ThisArg, memb.DisplayName with
    //    | Some callee, "Invoke" ->
    //        let callInfo = { callInfo with ThisArg = None }
    //        makeCall r typ callInfo callee
    //    | _ ->
    //        "Only Invoke is supported in delegates"
    //        |> addErrorAndReturnNull com ctx.InlinePath r

    //// Check if this is an interface or abstract/overriden method
    //| _, Some entity when
    //    entity.IsInterface && memb.IsInstanceMember
    //    || memb.IsOverrideOrExplicitInterfaceImplementation
    //    || memb.IsDispatchSlot
    //    ->

    //    // When calling `super` in an override, it may happen the method is not originally declared
    //    // by the immediate parent, so we need to go through the hierarchy until we find the original declaration
    //    // (this is important to get the correct mangled name)
    //    let entity =
    //        match memb.IsOverrideOrExplicitInterfaceImplementation, callInfo.ThisArg with
    //        | true, Some(Fable.Value(Fable.BaseValue _, _)) ->
    //            // Only compare param types for overloads (single curried parameter group)
    //            let paramTypes =
    //                if memb.CurriedParameterGroups.Count = 1 then
    //                    memb.CurriedParameterGroups[0]
    //                    |> Seq.map (fun p -> makeType Map.empty p.Type)
    //                    |> Seq.toArray
    //                    |> Some
    //                else
    //                    None

    //            entity
    //            |> tryFindBaseEntity (fun ent ->
    //                tryFindAbstractMember com ent memb.CompiledName memb.IsInstanceMember paramTypes
    //                |> Option.isSome
    //            )
    //            |> Option.defaultValue entity
    //        | _ -> entity

    //    callAttachedMember com ctx r typ callInfo entity memb

    //| _, Some entity when isModuleValueForCalls com entity memb ->
    //    let typ = makeType ctx.GenericArgs memb.FullType
    //    memberIdent com r typ memb membRef
    | _ ->
        // If member looks like a value but behaves like a function (has generic args) the type from F# AST is wrong (#2045).
        //let typ = makeType ctx.GenericArgs memb.FullType
        //let retTyp = makeType ctx.GenericArgs memb.ReturnParameter.Type
        //let callInfo = { callInfo with Tags = "value" :: callInfo.Tags }
        //let callExpr = memberIdent com r typ memb membRef |> makeCall r retTyp callInfo

        //let fableMember = FsMemberFunctionOrValue(memb)
        //// TODO: Move plugin application to FableTransforms
        //com.ApplyMemberCallPlugin(fableMember, callExpr)
        failwith "Could not match something else"
let makeCallFrom
    (typ: WatType)
    (genArgs: WatType list)
    callee
    args
    (memb: FSharpMemberOrFunctionOrValue)
    =
    //let ctx = addGenArgsToContext ctx memb genArgs
    let memberRef = getFunctionMemberRef memb

    WatAst.CallInfo.Create(
        ?thisArg = callee,
        args = transformOptionalArguments memb args,
        genArgs = genArgs,
        sigArgTypes = getArgTypes memb,
        // isCons = memb.IsConstructor,
        memberRef = memberRef
    )
    |> makeCallWithArgInfo typ callee memb memberRef


//let private getEntityMangledName trimRootModule (ent: EntityRef) =
//    let fullName = ent.FullName

//    match trimRootModule, ent.Path with
//    | TrimRootModule com, (Fable.SourcePath sourcePath | WatAst.PrecompiledLib(sourcePath, _)) ->
//        let rootMod, _ = com.GetRootModule(sourcePath)

//        if fullName.StartsWith(rootMod, StringComparison.Ordinal) then
//            fullName.Substring(rootMod.Length).TrimStart('.')
//        else
//            fullName
//    // Ignore entities for which we don't have implementation file data
//    | TrimRootModule _, (Fable.AssemblyPath _ | Fable.CoreAssemblyName _)
//    | NoTrimRootModule, _ -> fullName
//type TrimRootModule =
//    | TrimRootModule of Compiler
//    | NoTrimRootModule
//let private getEntityMangledName trimRootModule (ent: Fable.EntityRef) =
//    let fullName = ent.FullName

//    match trimRootModule, ent.Path with
//    | TrimRootModule com, (Fable.SourcePath sourcePath | Fable.PrecompiledLib(sourcePath, _)) ->
//        let rootMod, _ = com.GetRootModule(sourcePath)

//        if fullName.StartsWith(rootMod, StringComparison.Ordinal) then
//            fullName.Substring(rootMod.Length).TrimStart('.')
//        else
//            fullName
//    // Ignore entities for which we don't have implementation file data
//    | TrimRootModule _, (Fable.AssemblyPath _ | Fable.CoreAssemblyName _)
//    | NoTrimRootModule, _ -> fullName
let private getMemberMangledName (memb: FSharpMemberOrFunctionOrValue) =
    //if memb.IsExtensionMember then
    //    let overloadSuffix =
    //        memb.CurriedParameterGroups
    //        |> mapToList (mapToList (fun p -> makeType p.Type))
    //        |> getExtensionHash

    //    let entName =
    //        Ref memb.ApparentEnclosingEntity |> getEntityMangledName NoTrimRootModule

    //    entName, Naming.InstanceMemberPart(memb.CompiledName, overloadSuffix)
    //else
        match memb.DeclaringEntity with
        | Some ent ->
            let entRef = Ref ent
            let entName = entRef.FullName
            
            //if ent.IsFSharpModule then
            //entName, memb.CompiledName
            match entName with
            //| TrimRootModule com, _ when com.Options.Language = Rust -> memb.CompiledName, Naming.NoMemberPart // module prefix for Rust
            | "" -> memb.CompiledName, WatAst.NoMemberPart
            | moduleName -> moduleName, WatAst.StaticMemberPart(memb.CompiledName, "")
            //else
            //    let overloadSuffix = getOverloadSuffixFrom ent memb

            //    if memb.IsInstanceMember then
            //        entName, Naming.InstanceMemberPart(memb.CompiledName, overloadSuffix)
            //    else
            //        // Special case of non-mangled static classes to easily expose methods with optional args, etc, to native code
            //        // TODO: If entity is not mangled and Erase attribute is not present, raise warning
            //        match Util.tryMangleAttribute ent.Attributes with
            //        | Some false -> memb.CompiledName, Naming.NoMemberPart
            //        | Some true
            //        | None -> entName, Naming.StaticMemberPart(memb.CompiledName, overloadSuffix)
        | None -> memb.CompiledName, WatAst.NoMemberPart
let cleanNameAsJsIdentifier (name: string) =
    if name = ".ctor" then
        "$ctor"
    else
        name.Replace('.', '_').Replace('`', '$')
let isIdentChar index (c: char) =
    let code = int c

    c = '_'
    || c = '$'
    || (65 <= code && code <= 90) // a-z
    || (97 <= code && code <= 122) // A-Z
    // Digits are not allowed in first position, see #1397
    || (index > 0 && 48 <= code && code <= 57) // 0-9
    || Char.IsLetter c
let hasIdentForbiddenChars (ident: string) =
    let mutable found = false

    for i = 0 to ident.Length - 1 do
        found <- found || not (isIdentChar i ident.[i])

    found
let sanitizeIdentForbiddenCharsWith replace (ident: string) =
    if hasIdentForbiddenChars ident then
        Seq.init
            ident.Length
            (fun i ->
                let c = ident.[i]

                if isIdentChar i c then
                    string<char> c
                else
                    replace c
            )
        |> String.Concat
    else
        ident
let sanitizeIdentForbiddenChars (ident: string) =
    ident
    |> sanitizeIdentForbiddenCharsWith (fun c -> "$" + String.Format("{0:X}", int c).PadLeft(4, '0'))
let private printPart sanitize separator part overloadSuffix =
    (if part = "" then
            ""
        else
            separator + (sanitize part))
    + (if overloadSuffix = "" then
            ""
        else
            "_" + overloadSuffix)
let private buildName (sanitize: string -> string) name part =
    (sanitize name)
        + (
            match part with
            | InstanceMemberPart(s, i) -> printPart sanitize "__" s i
            | StaticMemberPart(s, i) -> printPart sanitize "_" s i
            | NoMemberPart -> ""
        )
let checkJsKeywords name =
    if jsKeywords.Contains name then
        name + "$"
    else
        name
let preventConflicts conflicts originalName =
    let rec check originalName n =
        let name =
            if n > 0 then
                originalName + "_" + (string<int> n)
            else
                originalName

        if not (conflicts name) then
            name
        else
            check originalName (n + 1)

    check originalName 0
let sanitizeIdent conflicts name part =
    // Replace Forbidden Chars
    buildName sanitizeIdentForbiddenChars name part
    |> checkJsKeywords
    // Check if it already exists
    |> preventConflicts conflicts
let getMemberDeclarationName (memb: FSharpMemberOrFunctionOrValue) =
    let name, part = getMemberMangledName memb

    let name, part =
        //match com.Options.Language, memb.DeclaringEntity with
        //| Rust, Some ent when memb.IsExtensionMember ->
        //    // For Rust, add entity prefix to extension methods
        //    cleanNameAsRustIdentifier name, part.Replace(cleanNameAsRustIdentifier)
        //| Rust, Some ent when ent.IsInterface && not memb.IsDispatchSlot ->
        //    // For Rust, add entity prefix to default static interface members
        //    cleanNameAsRustIdentifier name, part.Replace(cleanNameAsRustIdentifier)
        //| Rust, _ ->
        //    // for Rust, no entity prefix for other members
        //    memberNameAsRustIdentifier name part
        cleanNameAsJsIdentifier name, part.Replace(cleanNameAsJsIdentifier)

    let sanitizedName =
        //match com.Options.Language with
        //| Python ->
        //    let name =
        //        // Don't snake_case if member has compiled name attribute
        //        match memb.Attributes |> Helpers.tryFindAttrib Atts.compiledName with
        //        | Some _ -> name
        //        | _ -> Fable.Py.Naming.toSnakeCase name

        //    Fable.Py.Naming.sanitizeIdent Fable.Py.Naming.pyBuiltins.Contains name part
        //| Rust -> Naming.buildNameWithoutSanitation name part
        //| _ -> 
        sanitizeIdent (fun _ -> false) name part

    //let hasOverloadSuffix = not (String.IsNullOrEmpty(part.OverloadSuffix))
    sanitizedName //, hasOverloadSuffix


let hasOwnSignatureFile (ent: FSharpEntity) =
    not ent.IsNamespace
    && (
        match ent.SignatureLocation with
        | None -> false
        | Some m -> m.FileName.EndsWith(".fsi", StringComparison.Ordinal)
    )
let parentHasSignatureFile (declaringEntity: FSharpEntity option) =
    declaringEntity
    |> Option.map (fun ent -> hasOwnSignatureFile ent)
    |> Option.defaultValue false

let topLevelBindingHiddenBySignatureFile (v: FSharpMemberOrFunctionOrValue) =
    v.IsModuleValueOrMember
    && not v.HasSignatureFile
    && parentHasSignatureFile v.DeclaringEntity
let isNotPrivate (memb: FSharpMemberOrFunctionOrValue) =
    if memb.IsCompilerGenerated then
        false
    elif topLevelBindingHiddenBySignatureFile memb then
        false
    else
        not memb.Accessibility.IsPrivate




let makeValue value = Value(value)
let makeTypeConst (typ: WatAst.WatType) (value: obj) =
    match typ, value with
    //| Boolean, (:? bool as x) -> BoolConstant x |> makeValue r
    //| String, (:? string as x) -> StringConstant x |> makeValue r
    //| Char, (:? char as x) -> CharConstant x |> makeValue r
    | WatType.Number(kind), x ->

        // TODO : this needs some attention. Do kind and type of obj always match here? If not, should we cast early?

        match kind, x with
        //| Decimal, (:? decimal as x) -> NumberValue.Decimal x
        //| BigInt, (:? bigint as x) -> NumberValue.BigInt x
        //| Int64, (:? int64 as x) -> NumberValue.Int64 x
        //| UInt64, (:? uint64 as x) -> NumberValue.UInt64 x
        //| NativeInt, (:? nativeint as x) -> NumberValue.NativeInt x
        //| UNativeInt, (:? unativeint as x) -> NumberValue.UNativeInt x
        //| Int8, (:? int8 as x) -> NumberValue.Int8 x
        //| UInt8, (:? uint8 as x) -> NumberValue.UInt8 x
        //| Int16, (:? int16 as x) -> NumberValue.Int16 x
        //| UInt16, (:? uint16 as x) -> NumberValue.UInt16 x
        | Int32, (:? int32 as x) -> NumberValue.Int32 x
        //| UInt32, (:? uint32 as x) -> NumberValue.UInt32 x
        //| Float32, (:? float32 as x) -> NumberValue.Float32 x
        //| Float64, (:? float as x) -> NumberValue.Float64 x

        //| Int128, _
        //| UInt128, _
        //| Float16, _ ->
        //    FableError $"Unsupported Number Kind %A{kind} and value %A{x} combination"
        //    |> raise

        //| _ ->
        //    FableError $"Unexpected Number Kind %A{kind} and value %A{value} combination"
        //    |> raise
        | _ -> failwith $"Unexpected number kind: %A{kind}"

        |> fun value -> NumberConstant(value) |> makeValue

    //| Unit, _ -> UnitConstant |> makeValue r
    //// Arrays with small data type (ushort, byte) are represented
    //// in F# AST as BasicPatterns.Const
    //| Array(Number(kind, uom), arrayKind), (:? (byte[]) as arr) ->
    //    let values =
    //        arr
    //        |> Array.map (fun x -> NumberConstant(NumberValue.UInt8 x, uom) |> makeValue None)
    //        |> Seq.toList

    //    NewArray(ArrayValues values, Number(kind, uom), arrayKind) |> makeValue r
    //| Array(Number(kind, uom), arrayKind), (:? (uint16[]) as arr) ->
    //    let values =
    //        arr
    //        |> Array.map (fun x -> NumberConstant(NumberValue.UInt16 x, uom) |> makeValue None)
    //        |> Seq.toList

    //    NewArray(ArrayValues values, Number(kind, uom), arrayKind) |> makeValue r
    | _ ->
        failwith  $"Unexpected type %A{typ} for literal {value} (%s{value.GetType().FullName})"
/// This matches the boilerplate generated to wrap .NET events from F#
let (|CreateEvent|_|) =
    function
    | FSharpExprPatterns.Call(None,
            createEvent,
            _,
            _,
            [ FSharpExprPatterns.Lambda(_eventDelegate, FSharpExprPatterns.Call(Some callee, addEvent, [], [], [ FSharpExprPatterns.Value _eventDelegate' ]))
              FSharpExprPatterns.Lambda(_eventDelegate2, FSharpExprPatterns.Call(Some _callee2, _removeEvent, [], [], [ FSharpExprPatterns.Value _eventDelegate2' ]))
              FSharpExprPatterns.Lambda(_callback,
                    FSharpExprPatterns.NewDelegate(_,
                                FSharpExprPatterns.Lambda(_delegateArg0,
                                        FSharpExprPatterns.Lambda(_delegateArg1,
                                                FSharpExprPatterns.Application(FSharpExprPatterns.Value _callback',
                                                            [],
                                                            [ FSharpExprPatterns.Value _delegateArg0'; FSharpExprPatterns.Value _delegateArg1' ]))))) ]) when
        createEvent.FullName = Literals.createEvent
        ->
        let eventName = addEvent.CompiledName.Replace("add_", "")

        match addEvent.DeclaringEntity with
        | Some klass ->
            klass.MembersFunctionsAndValues
            |> Seq.tryFind (fun m -> m.LogicalName = eventName)
            |> function
                | Some memb -> Some(callee, memb)
                | _ -> None
        | _ -> None
    | _ -> None
let (|NestedLambda|_|) x =
    let rec nestedLambda args =
        function
        | FSharpExprPatterns.Lambda(arg, body) -> nestedLambda (arg :: args) body
        | body -> List.rev args, body

    match x with
    | FSharpExprPatterns.Lambda(arg, body) -> nestedLambda [ arg ] body |> Some
    | _ -> None
let rec private transformExpr appliedGenArgs fsExpr =
    match fsExpr with
    | FSharpExprPatterns.Const(value, typ) ->
        let typ = makeType typ
        let expr: WatAst.Expr = makeTypeConst typ value
        expr
    | FSharpExprPatterns.CallWithWitnesses(callee, memb, ownerGenArgs, memberGenArgs, witnesses, args) ->
        let typ = makeType fsExpr.Type
        let args = transformExprList args

        // Sometimes args may include local generics (e.g. an identifier referencing a local generic function)
        // so we try to match extract them by comparing the arg types with the expected types (from the member signature)
        //let args =
        //    let expectedArgTypes =
        //        let ctx = addGenArgsToContext ctx memb callGenArgs

        //        Seq.concat memb.CurriedParameterGroups
        //        |> Seq.map (fun x -> makeType ctx.GenericArgs x.Type)
        //        |> Seq.toList

        //    if List.sameLength args expectedArgTypes then
        //        List.zip args expectedArgTypes
        //        |> List.map (fun (argExpr, expectedArgType) ->
        //            extractGenericArgs argExpr expectedArgType |> replaceGenericArgs argExpr
        //        )
        //    else
        //        args
        match callee, memb with
        | Some(CreateEvent(callee, event) as createEvent), _ ->
            let callee = transformExpr [] callee
            let eventType = makeType createEvent.Type

            let callee =
                makeCallFrom eventType [] (Some callee) [] event

            makeCallFrom typ [] (Some callee) args memb

        //| Some unionExpr, UnionCaseTesterFor unionCase ->
        //    transformUnionCaseTest com ctx (makeRangeFrom fsExpr) unionExpr unionExpr.Type unionCase

        | callee, _ ->
            //let r = makeRangeFrom fsExpr
            let callee = transformExprOpt callee

            //let ctx =

            //    match witnesses with
            //    | [] -> []
            //    | witnesses ->
            //        let witnesses =
            //            witnesses
            //            |> List.choose (
            //                function
            //                // Index is not reliable, just append witnesses from parent call
            //                | FSharpExprPatterns.WitnessArg _idx -> None
            //                | NestedLambda(args, body) ->
            //                    match body with
            //                    | FSharpExprPatterns.Call(callee, memb, _, _, _args) ->
            //                        Some(memb.CompiledName, Option.isSome callee, args, body)
            //                    | FSharpExprPatterns.AnonRecordGet(_, calleeType, fieldIndex) ->
            //                        let fieldName = calleeType.AnonRecordTypeDetails.SortedFieldNames[fieldIndex]

            //                        Some("get_" + fieldName, true, args, body)
            //                    | FSharpExprPatterns.FSharpFieldGet(_, _, field) ->
            //                        Some("get_" + field.Name, true, args, body)
            //                    | _ -> None
            //                | _ -> None
            //            )

            //        // Seems witness act like a stack (that's why we reverse them)
            //        // so a witness may need other witnesses to be resolved

            //        List.rev witnesses
            //        |> List.fold (fun (traitName: string, isInstance: bool, args: FSharpMemberOrFunctionOrValue, body: FSharpExpr) ->
            //            let args = makeFunctionArgs args

            //            let body = transformExpr [] body

            //            let w: Fable.Witness =
            //                {
            //                    TraitName = traitName
            //                    IsInstance = isInstance
            //                    FileName = com.CurrentFile
            //                    Expr = Fable.Delegate(args, body, None, Fable.Tags.empty)
            //                }

            //            { ctx with Witnesses = w :: ctx.Witnesses }
            //        )

            makeCallFrom typ [] callee args memb
        //failwith "Cannot handle CallWithWitnesses Expr"
    | FSharpExprPatterns.Call(None, memb, _, _, [e1; e2]) ->
        
        let e1 = transformExpr appliedGenArgs e1
        let e2 = transformExpr [] e2
        let e = WatAst.Expr.Get(e1, GetKind.ExprGet e2, WatType.Any)
        let typ = makeType fsExpr.Type
        failwith "Cannot handle Call expression"
    | _ -> failwith "Unexpected Expression type"
let getValueMemberRef (memb: FSharpMemberOrFunctionOrValue) =
    match memb.DeclaringEntity with
    // We cannot retrieve compiler generated members from the entity
    | Some ent when not memb.IsCompilerGenerated ->
        let fableMemberFunctionOrValue =
            FsMemberFunctionOrValue(memb) :> WatAst.MemberFunctionOrValue

        let attributeFullNames =
            fableMemberFunctionOrValue.Attributes
            |> Seq.map (fun attr -> attr.Entity.FullName)
            |> List.ofSeq

        WatAst.MemberRef(
            Ref(ent),
            {
                CompiledName = memb.CompiledName
                IsInstance = memb.IsInstanceMember
                NonCurriedArgTypes = None
                AttributeFullNames = attributeFullNames
            }
        )
    | ent ->
        let entRef = ent |> Option.map Ref
        let typ = makeType memb.ReturnParameter.Type

        GeneratedMember.Value(
            memb.CompiledName,
            typ,
            isInstance = memb.IsInstanceMember,
            isMutable = memb.IsMutable,
            ?entRef = entRef
        )
let private transformMemberValue
    name
    (memb: FSharpMemberOrFunctionOrValue)
    (value: FSharpExpr) : WatAst.Declaration list
    =
    let value = transformExpr [] value
    
    [
        WatAst.MemberDeclaration
            {
                Name = name
                Args = [] //Kind = Fable.MemberValue(memb.IsMutable)
                Body = value
                //IsMangled = true
                MemberRef = getValueMemberRef memb
                //ImplementedSignatureRef = None
                //UsedNames = set ctx.UsedNamesInDeclarationScope
                //Tags = Fable.Tags.empty
                //XmlDoc = tryGetXmlDoc memb.XmlDoc
            }
    ]
    //let fableValue =
    //    if memb.IsMutable && isNotPrivate memb then
            
    //    else
    //        value

    //match value with
    //// Accept import expressions, e.g. let foo = import "foo" "myLib"
    //| Fable.Import(info, typ, r) when not info.IsCompilerGenerated ->
    //    match typ with
    //    | Fable.LambdaType(_, Fable.LambdaType _) ->
    //        "Change declaration of member: "
    //        + name
    //        + "\n"
    //        + "Importing functions with multiple arguments as `let add: int->int->int` won't uncurry parameters."
    //        + "\n"
    //        + "Use following syntax: `let add (x:int) (y:int): int = import ...`"
    //        |> addError com ctx.InlinePath None
    //    | _ -> ()

    //    let selector = importExprSelector memb info.Selector
    //    transformImportValue com r typ name memb selector info.Path
    //| fableValue ->
    //    // Mutable public values must be compiled as functions (see #986)
    //    // because values imported from ES2015 modules cannot be modified
    //    let fableValue =
    //        if memb.IsMutable && isNotPrivate memb then
    //            Replacements.Api.createMutablePublicValue com fableValue
    //        else
    //            fableValue

    //    [
    //        Fable.MemberDeclaration
    //            {
    //                Name = name
    //                Args = [] //Kind = Fable.MemberValue(memb.IsMutable)
    //                Body = fableValue
    //                IsMangled = true
    //                MemberRef = getValueMemberRef memb
    //                ImplementedSignatureRef = None
    //                UsedNames = set ctx.UsedNamesInDeclarationScope
    //                Tags = Fable.Tags.empty
    //                XmlDoc = tryGetXmlDoc memb.XmlDoc
    //            }
    //    ]
let makeIdentFrom (fsRef: FSharpMemberOrFunctionOrValue) : WatAst.Ident =
    let part = NoMemberPart

    let name =
        // The F# compiler sometimes adds a numeric suffix. Remove it because it's not deterministic.
        // See https://github.com/fable-compiler/Fable/issues/2869#issuecomment-1169574962
        if fsRef.IsCompilerGenerated then
            // Regex.Replace(fsRef.CompiledName, @"\d+$", "", RegexOptions.Compiled)
            fsRef.CompiledName.TrimEnd([| '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9' |])
        else
            fsRef.CompiledName

    let sanitizedName =
        sanitizeIdent (fun _ -> false) name part

    let isMutable = fsRef.IsMutable

    //ctx.UsedNamesInDeclarationScope.Add(sanitizedName) |> ignore
    //let r = makeRange fsRef.DeclarationLocation

    //let r =
    //    SourceLocation.Create(start = r.start, ``end`` = r.``end``, ?file = r.File, displayName = fsRef.DisplayName)

    {
        Name = sanitizedName
        Type = makeType fsRef.FullType
        IsThisArgument = fsRef.IsMemberThisValue
        IsCompilerGenerated = fsRef.IsCompilerGenerated
        IsMutable = isMutable
        //Range = Some r
    }

let putIdentInScope (fsRef: FSharpMemberOrFunctionOrValue) : WatAst.Ident =
    let ident = makeIdentFrom fsRef
    ident
let bindMemberArgs (args: FSharpMemberOrFunctionOrValue list list) =
    // The F# compiler "untuples" the args in methods
    let args = List.concat args

    let thisArg, args =
        match args with
        | firstArg :: restArgs when firstArg.IsMemberThisValue ->
            let thisArg = putIdentInScope firstArg
            let thisArg = { thisArg with IsThisArgument = true }
            [ thisArg ], restArgs
        | firstArg :: restArgs when firstArg.IsConstructorThisValue ->
            let thisArg = putIdentInScope firstArg
            let thisArg = { thisArg with IsThisArgument = true }
            [ thisArg ], restArgs
        | _ -> [], args

    let args =
        (([]), args)
        ||> List.fold (fun (accArgs) arg ->
            let arg = putIdentInScope arg
            arg :: accArgs
        )

    thisArg @ (List.rev args)
let private transformMemberFunction
    (name: string)
    (memb: FSharpMemberOrFunctionOrValue)
    args
    (body: FSharpExpr) : WatAst.Declaration list
    =
    let args = bindMemberArgs args
    let bodyExpr = transformExpr args body

    match bodyExpr with
    //// Accept import expressions, e.g. let foo x y = import "foo" "myLib"
    //| Fable.Import(info, _, r) when not info.IsCompilerGenerated ->
    //    // Use the full function type
    //    let typ = makeType Map.empty memb.FullType
    //    let selector = importExprSelector memb info.Selector

    //    let memberRef =
    //        // If this is a getter, it means the imported value is an object but Fable will call it as a function, see #2329
    //        if
    //            memb.IsPropertyGetterMethod // || (memb.IsFunction && com.Options.Language = Rust)
    //        then
    //            Fable.GeneratedMember.Function(name, [], typ)
    //        else
    //            Fable.GeneratedMember.Value(name, typ)

    //    transformImport com r typ name [] memberRef selector info.Path
    | body ->
        // If this is a static constructor, call it immediately
        //if memb.CompiledName = ".cctor" then
        //    [
        //        Fable.ActionDeclaration
        //            {
        //                Body =
        //                    Fable.Delegate(args, body, Some name, Fable.Tags.empty)
        //                    |> makeCall None Fable.Unit (makeCallInfo None [] [])
        //                UsedNames = set ctx.UsedNamesInDeclarationScope
        //            }
        //    ]
        //else
        let memberRef = getFunctionMemberRef memb

        [
            WatAst.MemberDeclaration
                {
                    Name = name
                    Args = args
                    Body = body
                    //UsedNames = set ctx.UsedNamesInDeclarationScope
                    //IsMangled = true
                    MemberRef = memberRef
                    //ImplementedSignatureRef = None
                    //Tags = Fable.Tags.empty
                    //XmlDoc = tryGetXmlDoc memb.XmlDoc
                }
        ]

let private transformMemberFunctionOrValue
    (memb: FSharpMemberOrFunctionOrValue)
    args
    (body: FSharpExpr) : WatAst.Declaration list
    =
    let name = getMemberDeclarationName memb
    //let name = ""
    if Helpers.isModuleValueForDeclarations memb then
        transformMemberValue name memb body
    else
        transformMemberFunction name memb args body
    
    //memb.Attributes
    //|> Seq.map (fun x -> FsAtt(x) :> Fable.Attribute)
    //|> function
    //    | ImportAtt(selector, path) ->
    //        let selector =
    //            if selector = Naming.placeholder then
    //                getMemberDisplayName memb
    //            else
    //                selector

    //        let typ = makeType Map.empty memb.FullType
    //        transformImportValue com None typ name memb selector path
    //    | _ ->
    //        if isModuleValueForDeclarations memb then
    //            transformMemberValue com ctx name memb body
    //        else
    //            transformMemberFunction com ctx name memb args body
    //let name, _ = getMemberDeclarationName memb

    //memb.Attributes
    //|> Seq.map (fun x -> FsAtt(x) :> Fable.Attribute)
    //|> function
    //    | ImportAtt(selector, path) ->
    //        let selector =
    //            if selector = Naming.placeholder then
    //                getMemberDisplayName memb
    //            else
    //                selector

    //        let typ = makeType Map.empty memb.FullType
    //        transformImportValue com None typ name memb selector path
    //    | _ ->
    //        if isModuleValueForDeclarations memb then
    //            transformMemberValue com ctx name memb body
    //        else
    //            transformMemberFunction com ctx name memb args body
let private transformMemberDecl
    (memb: FSharpMemberOrFunctionOrValue)
    (args: FSharpMemberOrFunctionOrValue list list)
    (body: FSharpExpr) : WatAst.Declaration list
    =
    match memb.DeclaringEntity with
    | Some ent when (isAttachMembersEntity ent && memb.CompiledName <> ".cctor") ->
        transformExplicitlyAttachedMember ent memb args body
        []
    | _ -> transformMemberFunctionOrValue memb args body

let rec transformDeclarations (fsDecls: FSharpImplementationFileDeclaration list): WatAst.Declaration list =
    fsDecls
    |> List.collect(fun fsDecl ->
        match fsDecl with
        | FSharpImplementationFileDeclaration.Entity (fsEnt, sub) ->
            match sub with
            | [] ->
                []
            | _ ->
                let members = transformDeclarations sub

                [
                    WatAst.ModuleDeclaration
                        {
                            Name = fsEnt.CompiledName
                            Members = members
                        }
                ]
        | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(memb, args, body) ->
            transformMemberDecl memb args body
        | FSharpImplementationFileDeclaration.InitAction fe ->
            let e = transformExpr [] fe

            [
                WatAst.ActionDeclaration
                    {
                        Body = e
                        //UsedNames = set ctx.UsedNamesInDeclarationScope
                    }
            ]
        | _ -> failwith "unexpected declaration type"
    )
let transformFile (input: string) =
    let checkProjectResults = parseAndCheckSingleFile input
    let checkedFile = checkProjectResults.AssemblyContents.ImplementationFiles.[0]

    //let declaration = checkedFile.Declarations.Head

    transformDeclarations checkedFile.Declarations

