module rec TinyFS.Core.WatAst

open FSharp.Compiler.Symbols
open System
open Transforms
open System.IO
open System.Collections.Generic

type EntityPath =
    | SourcePath of string
    | AssemblyPath of string
    /// Only used to reference entities in core assemblies without a path
    | CoreAssemblyName of string
    | PrecompiledLib of sourcePath: string * assemblyPath: string

type EntityRef =
    {
        FullName: string
        Path: EntityPath
    }

    member this.DisplayName =
        let name = this.FullName.Substring(this.FullName.LastIndexOf('.') + 1)

        match name.IndexOf('`') with
        | -1 -> name
        | i -> name.Substring(0, i)

    member this.SourcePath =
        match this.Path with
        | SourcePath p
        | PrecompiledLib(p, _) -> Some p
        | AssemblyPath _
        | CoreAssemblyName _ -> None
type Ident =
    {
        Name: string
        Type: WatType
        IsMutable: bool
        IsThisArgument: bool
        IsCompilerGenerated: bool
        //Range: SourceLocation option
    }

    member x.DisplayName =
        x.Name
type NumberValue =
    | Int32 of System.Int32
    | Int64 of System.Int64
type ValueKind =
    | NumberConstant of value: NumberValue
    | Null of typ: WatType

    member this.Type =
        match this with
        | NumberConstant value ->
            match value with
            | NumberValue.Int32 _ -> Number NumberKind.Int32
            | NumberValue.Int64 _ -> Number NumberKind.Int64
        | Null t -> t
type ArrayKind =
    | ResizeArray
    | MutableArray
    | ImmutableArray
// Operators
type UnaryOperator =
    | UnaryMinus
    | UnaryPlus
    | UnaryNot
    | UnaryNotBitwise
    | UnaryAddressOf

type BinaryOperator =
    | BinaryEqual
    | BinaryUnequal
    | BinaryLess
    | BinaryLessOrEqual
    | BinaryGreater
    | BinaryGreaterOrEqual
    | BinaryShiftLeft
    | BinaryShiftRightSignPropagating
    | BinaryShiftRightZeroFill
    | BinaryMinus
    | BinaryPlus
    | BinaryMultiply
    | BinaryDivide
    | BinaryModulus
    | BinaryExponent
    | BinaryOrBitwise
    | BinaryXorBitwise
    | BinaryAndBitwise

type LogicalOperator =
    | LogicalOr
    | LogicalAnd

type OperationKind =
    | Unary of operator: UnaryOperator * operand: Expr
    | Binary of operator: BinaryOperator * left: Expr * right: Expr
    | Logical of operator: LogicalOperator * left: Expr * right: Expr

type GetKind =
    | TupleIndex of index: int
    | ExprGet of expr: Expr
    | FieldGet of info: FieldInfo
    //| UnionField of info: UnionFieldInfo
    | UnionTag
    | ListHead
    | ListTail
    // TODO: Add isForced flag to distinguish between value accessed in pattern matching or not
    | OptionValue
type NumberKind =
    | Int32
    | Int64
type Expr =
    /// Identifiers that reference another expression
    | IdentExpr of ident: Ident
    /// Common and literal values
    | Value of kind: ValueKind
    // Closures
    /// Lambdas are curried, they always have a single argument (which can be unit)
    //| Lambda of arg: Ident * body: Expr * name: string option
    | Get of expr: Expr * kind: GetKind * typ: WatType
    | Call of callee: Expr * info: CallInfo * typ: WatType //* range: SourceLocation option
    | Operation of kind: OperationKind * tags: string list * typ: WatType

    member this.Type =
        match this with
        | IdentExpr id -> id.Type
        | Value kind -> kind.Type
        | Get(_, _, t)
        | Call(_, _, t)
        | Operation(_, _, t) -> t
type WatType =
    | Array of genericArg: WatType * kind: ArrayKind
    | Unit
    | Char
    | Number of kind: NumberKind
    | GenericParam of name: string * isMeasure: bool * constraints: Constraint list
    | Any

    member this.Generics =
        match this with
        | Array(gen, _) -> [gen]
        | Unit
        | Char
        | Number _
        | GenericParam _
        | Any -> []

type MemberDecl =
    {
        Name: string
        Args: Ident list
        Body: Expr
        MemberRef: MemberRef
    }
type ModuleDecl =
    {
        Name: string
        //Entity: EntityRef
        Members: Declaration list
    }

type ActionDecl =
    {
        Body: Expr
        //UsedNames: Set<string>
    }

and Declaration =
    | ModuleDeclaration of ModuleDecl
    | MemberDeclaration of MemberDecl    
    | ActionDeclaration of ActionDecl



(*
Somewhat of a logic break in the file
*)
type AbstractSignature =
    abstract Name: string
    abstract DeclaringType: WatType
type MemberFunctionOrValue =
    abstract DisplayName: string
    abstract CompiledName: string
    abstract FullName: string
    abstract Attributes: Attribute seq
    abstract HasSpread: bool
    abstract IsInline: bool
    abstract IsPublic: bool
    abstract IsPrivate: bool
    abstract IsInternal: bool
    abstract IsConstructor: bool
    abstract IsInstance: bool
    abstract IsExtension: bool
    abstract IsValue: bool
    abstract IsMutable: bool
    abstract IsGetter: bool
    abstract IsSetter: bool
    /// Indicates the member is a wrapper for a getter and/or setter,
    /// it evals to false for the actual getter/setter methods
    abstract IsProperty: bool
    abstract IsOverrideOrExplicitInterfaceImplementation: bool
    abstract IsDispatchSlot: bool
    abstract GenericParameters: GenericParam list
    abstract CurriedParameterGroups: Parameter list list
    abstract ReturnParameter: Parameter
    abstract DeclaringEntity: EntityRef option
    abstract ApparentEnclosingEntity: EntityRef option
    abstract ImplementedAbstractSignatures: AbstractSignature seq
    abstract XmlDoc: string option
type Attribute =
    abstract Entity: EntityRef
    abstract ConstructorArgs: obj list
type Constraint =
    | HasMember of name: string * isStatic: bool
    | CoercesTo of target: WatType
    | IsNullable
    | IsNotNullable
    | IsValueType
    | IsReferenceType
    | HasDefaultConstructor
    | HasAllowsRefStruct
    | HasComparison
    | HasEquality
    | IsUnmanaged
    | IsDelegate of argsType: WatType * retType: WatType
    | IsEnum of baseType: WatType
    | SimpleChoice of types: WatType list
type GenericParam =
    abstract Name: string
    abstract IsMeasure: bool
    abstract Constraints: Constraint list

type Parameter =
    abstract Attributes: Attribute seq
    abstract Name: string option
    abstract Type: WatType
    abstract IsIn: bool
    abstract IsOut: bool
    abstract IsNamed: bool
    abstract IsOptional: bool
    abstract DefaultValue: Expr option
type GeneratedMemberInfo =
    {
        Name: string
        ParamTypes: WatType list
        ReturnType: WatType
        IsInstance: bool
        HasSpread: bool
        IsMutable: bool
        DeclaringEntity: EntityRef option
    }
type GeneratedMember =
    | GeneratedFunction of info: GeneratedMemberInfo
    | GeneratedValue of info: GeneratedMemberInfo
    | GeneratedGetter of info: GeneratedMemberInfo
    | GeneratedSetter of info: GeneratedMemberInfo

    static member Function(name, paramTypes, returnType, ?isInstance, ?hasSpread, ?entRef) =
        {
            Name = name
            ParamTypes = paramTypes
            ReturnType = returnType
            IsInstance = defaultArg isInstance true
            HasSpread = defaultArg hasSpread false
            IsMutable = false
            DeclaringEntity = entRef
        }
        |> GeneratedFunction
        |> GeneratedMemberRef

    static member Value(name, typ, ?isInstance, ?isMutable, ?entRef) =
        {
            Name = name
            ParamTypes = []
            ReturnType = typ
            IsInstance = defaultArg isInstance true
            IsMutable = defaultArg isMutable false
            HasSpread = false
            DeclaringEntity = entRef
        }
        |> GeneratedValue
        |> GeneratedMemberRef

    static member Getter(name, typ, ?isInstance, ?entRef) =
        {
            Name = name
            ParamTypes = []
            ReturnType = typ
            IsInstance = defaultArg isInstance true
            IsMutable = false
            HasSpread = false
            DeclaringEntity = entRef
        }
        |> GeneratedGetter
        |> GeneratedMemberRef

    static member Setter(name, typ, ?isInstance, ?entRef) =
        {
            Name = name
            ParamTypes = [ typ ]
            ReturnType = Unit
            IsInstance = defaultArg isInstance true
            IsMutable = false
            HasSpread = false
            DeclaringEntity = entRef
        }
        |> GeneratedSetter
        |> GeneratedMemberRef

    member this.Info =
        match this with
        | GeneratedFunction info -> info
        | GeneratedValue info -> info
        | GeneratedGetter info -> info
        | GeneratedSetter info -> info

    static member Param(typ, ?name) =
        { new Parameter with
            member _.Attributes = []
            member _.Name = name
            member _.Type = typ
            member _.IsIn = false
            member _.IsOut = false
            member _.IsNamed = false
            member _.IsOptional = false
            member _.DefaultValue = None
        }

    static member GenericParams(typ: WatType) : GenericParam list =
        typ :: typ.Generics
        |> List.choose (
            function
            | GenericParam(name, isMeasure, constraints) ->
                { new GenericParam with
                    member _.Name = name
                    member _.IsMeasure = isMeasure
                    member _.Constraints = constraints
                }
                |> Some
            | _ -> None
        )

    interface MemberFunctionOrValue with
        member this.DeclaringEntity = this.Info.DeclaringEntity
        member this.DisplayName = this.Info.Name
        member this.CompiledName = this.Info.Name
        member this.FullName = this.Info.Name

        member this.GenericParameters =
            this.Info.ParamTypes |> List.collect (fun t -> GeneratedMember.GenericParams(t))

        member this.CurriedParameterGroups =
            [
                this.Info.ParamTypes
                |> List.mapi (fun i t -> GeneratedMember.Param(t, $"a%d{i}"))
            ]

        member this.ReturnParameter = GeneratedMember.Param(this.Info.ReturnType)

        member this.IsConstructor = this.Info.Name = ".ctor" || this.Info.Name = ".cctor"

        member this.IsInstance = this.Info.IsInstance
        member this.HasSpread = this.Info.HasSpread
        member this.IsMutable = this.Info.IsMutable

        member this.IsValue =
            match this with
            | GeneratedValue _ -> true
            | _ -> false

        member this.IsGetter =
            match this with
            | GeneratedGetter _ -> true
            | _ -> false

        member this.IsSetter =
            match this with
            | GeneratedSetter _ -> true
            | _ -> false

        member _.IsProperty = false
        member _.IsInline = false
        member _.IsPublic = true
        member _.IsPrivate = false
        member _.IsInternal = false
        member _.IsExtension = false
        member _.IsOverrideOrExplicitInterfaceImplementation = false
        member _.IsDispatchSlot = false
        member _.Attributes = []
        member _.ApparentEnclosingEntity = None
        member _.ImplementedAbstractSignatures = []
        member _.XmlDoc = None

type FieldInfo =
    {
        Name: string
        FieldType: Type option
        IsMutable: bool
        /// Indicates the field shouldn't be moved in beta reduction
        MaybeCalculated: bool
        Tags: string list
    }

    member this.CanHaveSideEffects = this.IsMutable || this.MaybeCalculated

    static member Create(name, ?fieldType: Type, ?isMutable: bool, ?maybeCalculated: bool, ?tag: string) =
        {
            Name = name
            FieldType = fieldType
            IsMutable = defaultArg isMutable false
            MaybeCalculated = defaultArg maybeCalculated false
            Tags = Option.toList tag
        }
        |> FieldGet
type MemberRefInfo =
    {
        IsInstance: bool
        CompiledName: string
        NonCurriedArgTypes: WatType list option
        // We only store the attributes fullname otherwise deserialization of precompiled files fails
        // System.Text.Json is not able to deserialize the standard Attribute type because it is an interface
        // More about it here: https://github.com/fable-compiler/Fable/pull/3817
        AttributeFullNames: string list
    }

type MemberRef =
    | MemberRef of declaringEntity: EntityRef * info: MemberRefInfo
    | GeneratedMemberRef of GeneratedMember
type CallInfo =
    {
        ThisArg: Expr option
        Args: Expr list
        /// Argument types as defined in the method signature, this may be slightly different to types of actual argument expressions.
        /// E.g.: signature accepts 'a->'b->'c (2-arity) but we pass int->int->int->int (3-arity)
        /// This is used for the uncurrying mechanism
        SignatureArgTypes: WatType list
        GenericArgs: WatType list
        MemberRef: MemberRef option
        Tags: string list
    }

    static member Create
        (
            ?thisArg: Expr,
            ?args: Expr list,
            ?genArgs: WatType list,
            ?sigArgTypes: WatType list,
            ?memberRef: MemberRef,
            ?isCons: bool,
            ?tag: string
        )
        =
        let tags = Option.toList tag

        {
            ThisArg = thisArg
            Args = defaultArg args []
            GenericArgs = defaultArg genArgs []
            SignatureArgTypes = defaultArg sigArgTypes []
            MemberRef = memberRef
            Tags =
                match isCons with
                | Some true -> "new" :: tags
                | Some false
                | None -> tags
        }
type ReplaceCallInfo =
    {
        CompiledName: string
        OverloadSuffix: string
        /// See ArgInfo.SignatureArgTypes
        SignatureArgTypes: WatType list
        HasSpread: bool
        IsModuleValue: bool
        IsInterface: bool
        DeclaringEntityFullName: string
        GenericArgs: WatType list
    }
let removeGetSetPrefix (s: string) =
    if
        s.StartsWith("get_", StringComparison.Ordinal)
        || s.StartsWith("set_", StringComparison.Ordinal)
    then
        s.Substring(4)
    else
        s
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
let isUnit (typ: FSharpType) =
    let typ = nonAbbreviatedType typ

    if typ.HasTypeDefinition then
        typ.TypeDefinition.TryFullName = Some Literals.unit
    else
        false
let countNonCurriedParams (meth: FSharpMemberOrFunctionOrValue) =
    let args = meth.CurriedParameterGroups

    if args.Count = 0 then
        0
    elif args[0].Count = 1 then
        if isUnit args[0].[0].Type then
            0
        else
            1
    else
        args[0].Count

/// ATTENTION: Make sure the ident name is unique
let makeTypedIdent typ name =
    {
        Name = name
        Type = typ
        IsCompilerGenerated = true
        IsThisArgument = false
        IsMutable = false
        //Range = None
    }

/// ATTENTION: Make sure the ident name is unique
let makeIdent name = makeTypedIdent Any name

/// ATTENTION: Make sure the ident name is unique
let makeIdentExpr name = makeIdent name |> IdentExpr

let getFieldWith t callee membName =
    Get(callee, FieldInfo.Create(membName, maybeCalculated = true), t)

let getField (e: Expr) membName = getFieldWith Any e membName
type Helper =
    static member GlobalCall
        (
            ident: string,
            returnType: WatType,
            args: Expr list,
            ?argTypes: WatType list,
            ?genArgs,
            ?memb: string,
            ?isConstructor: bool
        )
        =
        let callee =
            match memb with
            | Some memb -> getField (makeIdentExpr ident) memb
            | None -> makeIdentExpr ident

        let info =
            CallInfo.Create(args = args, ?sigArgTypes = argTypes, ?genArgs = genArgs, ?isCons = isConstructor)

        Call(callee, info, returnType)
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
    let ent = TinyFS.Core.Transforms.nonAbbreviatedDefinition ent

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
                let sourcePath = Transforms.SourcePath ent
                EntityPath.PrecompiledLib(sourcePath, normalizePath asmPath)
            //| dllName when Compiler.CoreAssemblyNames.Contains(dllName) -> Fable.CoreAssemblyName dllName
            | _ -> normalizePath asmPath |> EntityPath.AssemblyPath
        | None -> Transforms.SourcePath ent |> EntityPath.SourcePath

    {
        FullName = FullName ent
        Path = path
    }
type FsAtt(att: FSharpAttribute) =
    interface WatAst.Attribute with
        member _.Entity = Ref att.AttributeType
        member _.ConstructorArgs = att.ConstructorArguments |> Helpers.Seq.mapToList snd

type FsGenParam(gen: FSharpGenericParameter) =
    interface GenericParam with
        member _.Name = genParamName gen
        member _.IsMeasure = gen.IsMeasure
        member _.Constraints = FsGenParam.Constraints gen

    static member Constraint(c: FSharpGenericParameterConstraint) =
        if c.IsCoercesToConstraint then
            // It seems sometimes there are circular references so skip the constraints here
            let t = makeTypeWithConstraints false c.CoercesToTarget
            Constraint.CoercesTo t |> Some
        elif c.IsMemberConstraint then
            let d = c.MemberConstraintData // TODO: Full member signature hash?
            Constraint.HasMember(d.MemberName, d.MemberIsStatic) |> Some
        elif c.IsSupportsNullConstraint then
            Some Constraint.IsNullable
        //elif c.IsNotSupportsNullConstraint then
        //    Some Constraint.IsNotNullable
        elif c.IsNonNullableValueTypeConstraint then
            Some Constraint.IsValueType
        elif c.IsReferenceTypeConstraint then
            Some Constraint.IsReferenceType
        elif c.IsRequiresDefaultConstructorConstraint then
            Some Constraint.HasDefaultConstructor
        //elif c.IsAllowsRefStructConstraint then
        //    Some Constraint.HasAllowsRefStruct
        elif c.IsComparisonConstraint then
            Some Constraint.HasComparison
        elif c.IsEqualityConstraint then
            Some Constraint.HasEquality
        elif c.IsUnmanagedConstraint then
            Some Constraint.IsUnmanaged
        elif c.IsDelegateConstraint then
            let d = c.DelegateConstraintData

            let at =
                makeTypeWithConstraints false d.DelegateTupledArgumentType

            let rt = makeTypeWithConstraints false d.DelegateReturnType
            Constraint.IsDelegate(at, rt) |> Some
        elif c.IsEnumConstraint then
            let t = makeTypeWithConstraints false c.EnumConstraintTarget
            Constraint.IsEnum t |> Some
        elif c.IsSimpleChoiceConstraint then
            let types =
                c.SimpleChoices
                |> Seq.map (makeTypeWithConstraints false)
                |> Seq.toList

            Constraint.SimpleChoice types |> Some
        else
            None // TODO: Document these cases

    static member Constraints(gen: FSharpGenericParameter) =
        gen.Constraints |> Helpers.Seq.chooseToList FsGenParam.Constraint

type FsAbstractSignature(s: FSharpAbstractSignature) =
    interface AbstractSignature with
        member _.Name = s.Name
        member _.DeclaringType = makeType s.DeclaringType

type FsParam(p: FSharpParameter, ?isNamed) =
    let isOptional = p.IsOptionalArg

    let defValue =
        if isOptional then
            p.Attributes
            |> Transforms.tryFindAttrib "System.Runtime.InteropServices.DefaultParameterValueAttribute"
            |> Option.bind (fun att ->
                Seq.tryHead att.ConstructorArguments
                |> Option.map (fun (t, v) ->
                    if isNull v then
                        makeType t |> makeNullTyped
                    else
                        makeConstFromObj v
                )
            )
        else
            None

    interface WatAst.Parameter with
        member _.Name = p.Name
        member _.Type = makeType p.Type

        member _.Attributes = p.Attributes |> Seq.map (fun x -> FsAtt(x) :> WatAst.Attribute)

        member _.IsIn = p.IsInArg
        member _.IsOut = p.IsOutArg
        member _.IsNamed = defaultArg isNamed false
        member _.IsOptional = isOptional
        member _.DefaultValue = defValue
type FsMemberFunctionOrValue(m: FSharpMemberOrFunctionOrValue) =
    static member CompiledName(m: FSharpMemberOrFunctionOrValue) =
        if FsMemberFunctionOrValue.IsGetter(m) || FsMemberFunctionOrValue.IsSetter(m) then
            removeGetSetPrefix m.CompiledName
        else
            m.CompiledName

    static member DisplayName(m: FSharpMemberOrFunctionOrValue) =
        if FsMemberFunctionOrValue.IsGetter(m) || FsMemberFunctionOrValue.IsSetter(m) then
            removeGetSetPrefix m.DisplayNameCore
        else
            m.DisplayNameCore

    // We don't consider indexer properties as getters/setters so they're always compiled as methods
    static member IsGetter(m: FSharpMemberOrFunctionOrValue) =
        m.IsPropertyGetterMethod && countNonCurriedParams m = 0

    static member IsSetter(m: FSharpMemberOrFunctionOrValue) =
        m.IsPropertySetterMethod && countNonCurriedParams m = 1

    interface MemberFunctionOrValue with
        member _.Attributes = m.Attributes |> Seq.map (fun x -> FsAtt(x) :> WatAst.Attribute)

        member _.CurriedParameterGroups =
            let mutable i = -1

            let namedParamsIndex =
                m.Attributes
                |> Transforms.tryFindAttrib Literals.paramObject
                |> Option.map (fun (att: FSharpAttribute) ->
                    match Seq.tryItem 0 att.ConstructorArguments with
                    | Some(_, (:? int as index)) -> index
                    | _ -> 0
                )

            m.CurriedParameterGroups
            |> Helpers.Seq.mapToList (
                Helpers.Seq.mapToList (fun p ->
                    i <- i + 1

                    let isNamed =
                        match namedParamsIndex with
                        | Some namedParamsIndex -> i >= namedParamsIndex
                        | None -> false

                    FsParam(p, isNamed = isNamed) :> WatAst.Parameter
                )
            )

        member _.HasSpread = Helpers.hasParamArray m
        member _.IsInline = Helpers.isInline m
        member _.IsPublic = Helpers.isNotPrivate m
        member _.IsPrivate = m.Accessibility.IsPrivate
        member _.IsInternal = m.Accessibility.IsInternal
        // Using memb.IsValue doesn't work for function values
        // (e.g. `let ADD = adder()` when adder returns a function)
        member _.IsValue = Helpers.isModuleValueForDeclarations m
        member _.IsDispatchSlot = m.IsDispatchSlot
        member _.IsConstructor = m.IsConstructor
        member _.IsInstance = m.IsInstanceMember
        member _.IsExtension = m.IsExtensionMember
        member _.IsMutable = m.IsMutable
        member _.IsProperty = m.IsProperty
        member _.IsGetter = FsMemberFunctionOrValue.IsGetter(m)
        member _.IsSetter = FsMemberFunctionOrValue.IsSetter(m)

        member _.IsOverrideOrExplicitInterfaceImplementation =
            m.IsOverrideOrExplicitInterfaceImplementation

        member _.DisplayName = FsMemberFunctionOrValue.DisplayName m
        member _.CompiledName = m.CompiledName
        member _.FullName = m.FullName

        member _.GenericParameters =
            m.GenericParameters |> Helpers.Seq.mapToList (fun p -> FsGenParam(p))

        member _.ReturnParameter = FsParam(m.ReturnParameter) :> WatAst.Parameter

        member _.ImplementedAbstractSignatures =
            m.ImplementedAbstractSignatures |> Seq.map (fun s -> FsAbstractSignature(s))

        member _.ApparentEnclosingEntity = Ref m.ApparentEnclosingEntity |> Some

        member _.DeclaringEntity = m.DeclaringEntity |> Option.map Ref
        member _.XmlDoc = tryGetXmlDoc m.XmlDoc


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

let makeTypeFromDef withConstraints (genArgs: IList<FSharpType>) (tdef: FSharpEntity) : WatAst.WatType =
    if tdef.IsArrayType then
        //Fable.Array(
        //    makeTypeGenArgsWithConstraints withConstraints ctxTypeArgs genArgs |> List.head,
        //    Fable.MutableArray
        //)
        //let numberValue = NumberValue.Int32(2)
        let vall = NumberKind.Int32
        WatType.Array(WatType.Number(vall), ArrayKind.MutableArray)
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
        | Literals.unit -> WatType.Unit
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
        | Patterns.DicContains numberTypes kind -> WatType.Number(kind)
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
let rec makeTypeWithConstraints (withConstraints: bool) (NonAbbreviatedType t) =
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
let makeType (t: FSharpType): WatType =
    makeTypeWithConstraints true t
let makeNullTyped t = Value(Null t)

let makeValue value = Value(value)
let makeConstFromObj (value: obj) =
    match value with
    //| :? bool as x -> BoolConstant x |> makeValue None
    //| :? string as x -> StringConstant x |> makeValue None
    //| :? char as x -> CharConstant x |> makeValue None
    // Integer types
    //| :? int8 as x -> NumberConstant(NumberValue.Int8 x) |> makeValue None
    //| :? uint8 as x -> NumberConstant(NumberValue.UInt8 x, NumberInfo.Empty) |> makeValue None
    //| :? int16 as x -> NumberConstant(NumberValue.Int16 x, NumberInfo.Empty) |> makeValue None
    //| :? uint16 as x -> NumberConstant(NumberValue.UInt16 x, NumberInfo.Empty) |> makeValue None
    | :? int32 as x -> NumberConstant(NumberValue.Int32 x) |> makeValue
    //| :? uint32 as x -> NumberConstant(NumberValue.UInt32 x, NumberInfo.Empty) |> makeValue None
    | :? int64 as x -> NumberConstant(NumberValue.Int64 x) |> makeValue
    //| :? uint64 as x -> NumberConstant(NumberValue.UInt64 x, NumberInfo.Empty) |> makeValue None
    // Float types
    //| :? float32 as x -> NumberConstant(NumberValue.Float32 x, NumberInfo.Empty) |> makeValue None
    //| :? float as x -> NumberConstant(NumberValue.Float64 x, NumberInfo.Empty) |> makeValue None
    //| :? decimal as x -> NumberConstant(NumberValue.Decimal x, NumberInfo.Empty) |> makeValue None
    | _ ->
        failwith $"Cannot create expression for object {value} (%s{value.GetType().FullName})"

///This exists somewhere else
let genParamName (genParam: FSharpGenericParameter) =
    // Sometimes the names of user-declared and compiler-generated clash, see #1900 and https://github.com/dotnet/fsharp/issues/13062
    let name = genParam.Name.Replace("?", "$")

    if genParam.IsCompilerGenerated then
        "$" + name
    else
        name
let tryGetXmlDoc =
    function
    | FSharpXmlDoc.FromXmlText(xmlDoc) -> xmlDoc.GetXmlText() |> Some
    | _ -> None

type MemberPart =
    | InstanceMemberPart of memberCompiledName: string * overloadSuffix: string
    | StaticMemberPart of memberCompiledName: string * overloadSuffix: string
    | NoMemberPart

    member this.Replace(f: string -> string) =
        match this with
        | InstanceMemberPart(s, o) -> InstanceMemberPart(f s, o)
        | StaticMemberPart(s, o) -> StaticMemberPart(f s, o)
        | NoMemberPart -> this

    member this.OverloadSuffix =
        match this with
        | InstanceMemberPart(_, o)
        | StaticMemberPart(_, o) -> o
        | NoMemberPart -> ""