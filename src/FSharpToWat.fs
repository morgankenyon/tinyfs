module Fado.Core.FSharpToWat

open Fado.Core
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text
open System.IO
open System.Collections.Generic
open WatAst
open System

let checker = FSharpChecker.Create(keepAssemblyContents=true)
type ParamTypes = WatType list

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
let numberTypes =
    dict
        [
            //Types.int8, Int8
            //Types.uint8, UInt8
            //Types.int16, Int16
            //Types.uint16, UInt16
            Literals.int32, NumberKind.Int32
            //Types.uint32, UInt32
            //Types.int64, Int64
            //Types.uint64, UInt64
            //Types.int128, Int128
            //Types.uint128, UInt128
            //Types.nativeint, NativeInt
            //Types.unativeint, UNativeInt
            //Types.float16, Float16
            //Types.float32, Float32
            //Types.float64, Float64
            //Types.decimal, Decimal
            //Types.bigint, BigInt
        ]
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
let isModuleValueForDeclarations (memb: FSharpMemberOrFunctionOrValue) =
    memb.CurriedParameterGroups.Count = 0 && memb.GenericParameters.Count = 0
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
let rec nonAbbreviatedType (t: FSharpType) : FSharpType =
    let isSameType (t1: FSharpType) (t2: FSharpType) =
        t1.HasTypeDefinition
        && t2.HasTypeDefinition
        && (t1.TypeDefinition = t2.TypeDefinition)

    if t.IsAbbreviation && not (isSameType t t.AbbreviatedType) then
        nonAbbreviatedType t.AbbreviatedType
    elif t.HasTypeDefinition then
        let abbr = t.AbbreviatedType
        // .IsAbbreviation doesn't eval to true for generic numbers
        // See https://github.com/Microsoft/visualfsharp/issues/5992
        if t.GenericArguments.Count = abbr.GenericArguments.Count then
            t
        else
            abbr
    else
        t

let rec nonAbbreviatedDefinition (ent: FSharpEntity) : FSharpEntity =
    if ent.IsFSharpAbbreviation then
        let t = ent.AbbreviatedType

        if t.HasTypeDefinition && t.TypeDefinition <> ent then
            nonAbbreviatedDefinition t.TypeDefinition
        else
            ent
    else
        ent
let inline (|NonAbbreviatedType|) (t: FSharpType) = nonAbbreviatedType t
let (|DicContains|_|) (dic: System.Collections.Generic.IDictionary<'k, 'v>) key =
    let success, value = dic.TryGetValue key

    if success then
        Some value
    else
        None

let tryArrayFullName (ent: FSharpEntity) =
    if ent.IsArrayType then
        let rank =
            match ent.ArrayRank with
            | rank when rank > 1 -> "`" + string<int> rank
            | _ -> ""

        Some("System.Array" + rank)
    else
        None

//let rec nonAbbreviatedDefinition (ent: FSharpEntity) : FSharpEntity =
//    if ent.IsFSharpAbbreviation then
//        let t = ent.AbbreviatedType

//        if t.HasTypeDefinition && t.TypeDefinition <> ent then
//            nonAbbreviatedDefinition t.TypeDefinition
//        else
//            ent
//    else
//        ent

let ensureFsExtension (path: string) =
    if path.EndsWith(".fsi", StringComparison.Ordinal) then
        path.Substring(0, path.Length - 1)
    else
        path
let normalizePath (path: string) = path.Replace('\\', '/').TrimEnd('/')
let normalizePathAndEnsureFsExtension (path: string) = normalizePath path |> ensureFsExtension
let SourcePath(ent: FSharpEntity) =
    let ent = nonAbbreviatedDefinition ent

    ent.DeclarationLocation.FileName |> normalizePathAndEnsureFsExtension
let FullName(ent: FSharpEntity) : string =
    let ent = nonAbbreviatedDefinition ent

    match tryArrayFullName ent with
    | Some fullName -> fullName
    | None when ent.IsNamespace || ent.IsByRef ->
        match ent.Namespace with
        | Some ns -> ns + "." + ent.CompiledName
        | None -> ent.CompiledName
#if !FABLE_COMPILER
    | None when ent.IsProvided -> ent.LogicalName
#endif
    | None ->
        match ent.TryFullName with
        | Some n -> n
        | None -> ent.LogicalName
let Ref(ent: FSharpEntity) : WatAst.EntityRef =
    let ent = nonAbbreviatedDefinition ent

    let path =
        match ent.Assembly.FileName with
        | Some asmPath ->
            let dllName = Path.GetFileName(asmPath)
            let dllName = dllName.Substring(0, dllName.Length - 4) // Remove .dll extension

            match dllName with
            // When compiling with netcoreapp target, netstandard only contains redirects
            // We can find the actual assembly name from the entity qualified name
            | "netstandard" -> ent.QualifiedName.Split(',').[1].Trim() |> EntityPath.CoreAssemblyName
            | Literals.fablePrecompile ->
                let sourcePath = SourcePath ent
                EntityPath.PrecompiledLib(sourcePath, normalizePath asmPath)
            //| dllName when Compiler.CoreAssemblyNames.Contains(dllName) -> Fable.CoreAssemblyName dllName
            | _ -> normalizePath asmPath |> EntityPath.AssemblyPath
        | None -> SourcePath ent |> EntityPath.SourcePath

    {
        FullName = FullName ent
        Path = path
    }

let makeTypeFromDef withConstraints (genArgs: IList<FSharpType>) (tdef: FSharpEntity) : WatAst.WatType =
    if tdef.IsArrayType then
        //Fable.Array(
        //    makeTypeGenArgsWithConstraints withConstraints ctxTypeArgs genArgs |> List.head,
        //    Fable.MutableArray
        //)
        WatType.Array
    //elif tdef.IsDelegate then
    //    makeTypeFromDelegate withConstraints ctxTypeArgs genArgs tdef
    //elif tdef.IsEnum then
    //    // F# seems to include a field with this name in the underlying type
    //    let numberKind =
    //        tdef.FSharpFields
    //        |> Seq.tryPick (fun fi ->
    //            match fi.Name with
    //            | "value__" when fi.FieldType.HasTypeDefinition ->
    //                match FsEnt.FullName fi.FieldType.TypeDefinition with
    //                | DicContains numberTypes kind -> Some kind
    //                | _ -> None
    //            | _ -> None
    //        )
    //        |> Option.defaultValue Int32

    //    let info = FsEnt.Ref tdef |> Fable.NumberInfo.IsEnum
    //    Fable.Number(numberKind, info)
    else
        match FullName tdef with
        // Fable "primitives"
        //| Types.object -> Fable.Any
        //| Types.unit -> Fable.Unit
        //| Types.bool -> Fable.Boolean
        //| Types.char -> Fable.Char
        //| Types.string -> Fable.String
        //| Types.regex -> Fable.Regex
        //| Types.type_ -> Fable.MetaType
        //| Types.valueOption ->
        //    Fable.Option(makeTypeGenArgsWithConstraints withConstraints ctxTypeArgs genArgs |> List.head, true)
        //| Types.option ->
        //    Fable.Option(makeTypeGenArgsWithConstraints withConstraints ctxTypeArgs genArgs |> List.head, false)
        //| Types.resizeArray ->
        //    Fable.Array(
        //        makeTypeGenArgsWithConstraints withConstraints ctxTypeArgs genArgs |> List.head,
        //        Fable.ResizeArray
        //    )
        //| Types.list ->
        //    makeTypeGenArgsWithConstraints withConstraints ctxTypeArgs genArgs
        //    |> List.head
        //    |> Fable.List
        | DicContains numberTypes kind -> WatType.Number(kind)
        //| DicContains numbersWithMeasure kind ->
        //    let info = getMeasureFullName genArgs |> Fable.NumberInfo.IsMeasure

        //    Fable.Number(kind, info)
        //| DicContains runtimeTypesWithMeasure choice ->
        //    match choice with
        //    | Choice1Of2 t -> t
        //    | Choice2Of2 fullName -> makeRuntimeTypeWithMeasure genArgs fullName
        //| fullName when tdef.IsMeasure -> Fable.Measure fullName
        //| _ when hasAttrib Atts.stringEnum tdef.Attributes && Compiler.Language <> TypeScript -> Fable.String
        //| _ ->
        //    let genArgs = makeTypeGenArgsWithConstraints withConstraints ctxTypeArgs genArgs

        //    Fable.DeclaredType(FsEnt.Ref tdef, genArgs)
        | _ -> failwith "Unexpected type makeTypeFromDef"
    //WatAst.WatType.Any
let rec makeTypeWithConstraints withConstraints (NonAbbreviatedType t) =
    let typ =
        // Generic parameter (try to resolve for inline functions)
        //if t.IsGenericParameter then
        //    resolveGenParam withConstraints ctxTypeArgs t.GenericParameter
        // Tuple
        //elif t.IsTupleType then
        //    let genArgs =
        //        makeTypeGenArgsWithConstraints withConstraints ctxTypeArgs t.GenericArguments

        //    Fable.Tuple(genArgs, t.IsStructTupleType)
        // Function
        //if t.IsFunctionType then
        //    let argType =
        //        makeTypeWithConstraints withConstraints ctxTypeArgs t.GenericArguments[0]

        //    let returnType =
        //        makeTypeWithConstraints withConstraints ctxTypeArgs t.GenericArguments[1]

        //    Fable.LambdaType(argType, returnType)
        //elif t.IsAnonRecordType then
        //    let genArgs =
        //        makeTypeGenArgsWithConstraints withConstraints ctxTypeArgs t.GenericArguments

        //    let fields = t.AnonRecordTypeDetails.SortedFieldNames

        //    let isStruct =
        //        match t.BaseType with
        //        | Some typ -> (getFsTypeFullName typ) = Types.valueType
        //        | None -> false

        //    Fable.AnonymousRecordType(fields, genArgs, isStruct)
        if t.HasTypeDefinition then
            // No support for provided types when compiling FCS+Fable to JS
#if !FABLE_COMPILER
            // TODO: Discard provided generated types too?
            if t.TypeDefinition.IsProvidedAndErased then
                WatAst.Any
            else
#endif
            makeTypeFromDef withConstraints t.GenericArguments t.TypeDefinition
        //elif t.IsMeasureType then
        //    Fable.Measure ""
        else
            WatAst.Any // failwithf "Unexpected non-declared F# type: %A" t

    // TODO:
    // if not t.IsGenericParameter && t.HasNullAnnotation // || t.IsNullAmbivalent
    // then
    //     makeRuntimeType [ typ ] Types.nullable // represent it as Nullable<T>
    // else typ
    typ
let makeType t =
    makeTypeWithConstraints true t

let mapToList (f: 'a -> 'b) (xs: 'a seq) : 'b list =
    ([], xs) ||> Seq.fold (fun li x -> (f x) :: li) |> List.rev
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
            entName, memb.CompiledName
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
        | None -> memb.CompiledName, ""//, Naming.NoMemberPart
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
let private buildName (sanitize: string -> string) name part =
    (sanitize name) + "_" + (sanitize part)
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
        cleanNameAsJsIdentifier name, cleanNameAsJsIdentifier part

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
let rec private transformExpr appliedGenArgs fsExpr =
    match fsExpr with
    | FSharpExprPatterns.Const(value, typ) ->
        let typ = makeType typ
        let expr: WatAst.Expr = makeTypeConst typ value
        expr
    | FSharpExprPatterns.CallWithWitnesses(callee, memb, ownerGenArgs, memberGenArgs, witnesses, args) ->
        failwith "Cannot handle CallWithWitnesses Expr"
    | FSharpExprPatterns.Call(None, memb, _, _, [e1; e2]) ->
        
        let e1 = transformExpr appliedGenArgs e1
        let e2 = transformExpr [] e2
        let e = WatAst.Expr.Get(e1, GetKind.ExprGet e2, WatType.Any)
        let typ = makeType fsExpr.Type
        failwith "Cannot handle Call expression"
    | _ -> failwith "Unexpected Expression type"
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
                //Args = [] //Kind = Fable.MemberValue(memb.IsMutable)
                Body = value
                //IsMangled = true
                //MemberRef = getValueMemberRef memb
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

let private transformMemberFunction
    (name: string)
    (memb: FSharpMemberOrFunctionOrValue)
    args
    (body: FSharpExpr) : WatAst.Declaration list
    =
    []
    //let bodyCtx, args = bindMemberArgs com ctx args
    //let body = transformExpr com bodyCtx [] body |> run

    //match body with
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
    //| body ->
    //    // If this is a static constructor, call it immediately
    //    if memb.CompiledName = ".cctor" then
    //        [
    //            Fable.ActionDeclaration
    //                {
    //                    Body =
    //                        Fable.Delegate(args, body, Some name, Fable.Tags.empty)
    //                        |> makeCall None Fable.Unit (makeCallInfo None [] [])
    //                    UsedNames = set ctx.UsedNamesInDeclarationScope
    //                }
    //        ]
    //    else
    //        let body, memberRef =
    //            match com.Options.Language with
    //            | JavaScript
    //            | TypeScript
    //            | Python ->
    //                match applyJsPyDecorators com ctx name memb args body with
    //                | Some body ->
    //                    body, Fable.GeneratedMember.Value(name, body.Type, isInstance = memb.IsInstanceMember)
    //                | None -> body, getFunctionMemberRef memb
    //            | _ -> body, getFunctionMemberRef memb

    //        [
    //            Fable.MemberDeclaration
    //                {
    //                    Name = name
    //                    Args = args
    //                    Body = body
    //                    UsedNames = set ctx.UsedNamesInDeclarationScope
    //                    IsMangled = true
    //                    MemberRef = memberRef
    //                    ImplementedSignatureRef = None
    //                    Tags = Fable.Tags.empty
    //                    XmlDoc = tryGetXmlDoc memb.XmlDoc
    //                }
    //        ]

let private transformMemberFunctionOrValue
    (memb: FSharpMemberOrFunctionOrValue)
    args
    (body: FSharpExpr) : WatAst.Declaration list
    =
    let name = getMemberDeclarationName memb
    //let name = ""
    if isModuleValueForDeclarations memb then
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

let rec transformDeclaration (fsDecl: FSharpImplementationFileDeclaration): WatAst.Declaration list =
    match fsDecl with
    | FSharpImplementationFileDeclaration.Entity (fsEnt, sub) ->
        match sub with
        | [] ->
            []
        | _ -> 
            let firstMember = sub.Head

            let members = transformDeclaration firstMember

            [
                WatAst.ModuleDeclaration
                    {
                        Name = fsEnt.CompiledName
                        Members = members
                    }
            ]
    | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(memb, args, body) ->
        transformMemberDecl memb args body
    | _ -> failwith "unexpected declaration type"
let transformFile (input: string) =
    let checkProjectResults = parseAndCheckSingleFile input
    let checkedFile = checkProjectResults.AssemblyContents.ImplementationFiles.[0]

    let declaration = checkedFile.Declarations.Head

    transformDeclaration declaration

