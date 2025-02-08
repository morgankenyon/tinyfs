module Fado.Core.FSharpToWat

open Fado.Core
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text
open System.IO
open System.Collections.Generic
open WatAst

let checker = FSharpChecker.Create(keepAssemblyContents=true)

let parseAndCheckSingleFile (input: string) = 
    let file = Path.ChangeExtension(System.IO.Path.GetTempFileName(), "fsx")  
    File.WriteAllText(file, input)
    // Get context representing a stand-alone (script) file
    let projOptions, _errors = 
        checker.GetProjectOptionsFromScript(file, SourceText.ofString input, assumeDotNetFramework=false)
        |> Async.RunSynchronously

    checker.ParseAndCheckProject(projOptions) 
    |> Async.RunSynchronously

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
let inline (|NonAbbreviatedType|) (t: FSharpType) = nonAbbreviatedType t

let makeTypeFromDef withConstraints (genArgs: IList<FSharpType>) (tdef: FSharpEntity) : WatAst.WatType=
        //if tdef.IsArrayType then
        //    Fable.Array(
        //        makeTypeGenArgsWithConstraints withConstraints ctxTypeArgs genArgs |> List.head,
        //        Fable.MutableArray
        //    )
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
        //else
        //    match FsEnt.FullName tdef with
        //    // Fable "primitives"
        //    | Types.object -> Fable.Any
        //    | Types.unit -> Fable.Unit
        //    | Types.bool -> Fable.Boolean
        //    | Types.char -> Fable.Char
        //    | Types.string -> Fable.String
        //    | Types.regex -> Fable.Regex
        //    | Types.type_ -> Fable.MetaType
        //    | Types.valueOption ->
        //        Fable.Option(makeTypeGenArgsWithConstraints withConstraints ctxTypeArgs genArgs |> List.head, true)
        //    | Types.option ->
        //        Fable.Option(makeTypeGenArgsWithConstraints withConstraints ctxTypeArgs genArgs |> List.head, false)
        //    | Types.resizeArray ->
        //        Fable.Array(
        //            makeTypeGenArgsWithConstraints withConstraints ctxTypeArgs genArgs |> List.head,
        //            Fable.ResizeArray
        //        )
        //    | Types.list ->
        //        makeTypeGenArgsWithConstraints withConstraints ctxTypeArgs genArgs
        //        |> List.head
        //        |> Fable.List
        //    | DicContains numberTypes kind -> Fable.Number(kind, Fable.NumberInfo.Empty)
        //    | DicContains numbersWithMeasure kind ->
        //        let info = getMeasureFullName genArgs |> Fable.NumberInfo.IsMeasure

        //        Fable.Number(kind, info)
        //    | DicContains runtimeTypesWithMeasure choice ->
        //        match choice with
        //        | Choice1Of2 t -> t
        //        | Choice2Of2 fullName -> makeRuntimeTypeWithMeasure genArgs fullName
        //    | fullName when tdef.IsMeasure -> Fable.Measure fullName
        //    | _ when hasAttrib Atts.stringEnum tdef.Attributes && Compiler.Language <> TypeScript -> Fable.String
        //    | _ ->
        //        let genArgs = makeTypeGenArgsWithConstraints withConstraints ctxTypeArgs genArgs

        //        Fable.DeclaredType(FsEnt.Ref tdef, genArgs)
        WatAst.WatType.Any
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
let private transformExpr appliedGenArgs fsExpr =
    match fsExpr with
    | FSharpExprPatterns.Const(value, typ) ->
        let typ = makeType typ
        let expr: WatAst.Expr = makeTypeConst typ value
        expr
    | _ -> failwith "Unxepcetd Expression type"
let private transformMemberValue
    name
    (memb: FSharpMemberOrFunctionOrValue)
    (value: FSharpExpr) : WatAst.Declaration list
    =
    let value = transformExpr [] value
    []
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
    //let name, _ = getMemberDeclarationName memb
    let name = ""
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

