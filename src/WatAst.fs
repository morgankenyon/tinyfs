module rec Fado.Core.WatAst

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
type NumberValue =
    | Int32 of System.Int32
type ValueKind =
    | NumberConstant of value: NumberValue
type GetKind =
    | TupleIndex of index: int
    | ExprGet of expr: Expr
    //| FieldGet of info: FieldInfo
    //| UnionField of info: UnionFieldInfo
    | UnionTag
    | ListHead
    | ListTail
    // TODO: Add isForced flag to distinguish between value accessed in pattern matching or not
    | OptionValue
type NumberKind =
    | Int32
type Expr =
    | Value of kind: ValueKind
    | Get of expr: Expr * kind: GetKind * typ: WatType
type WatType =
    | Any
    | Number of kind: NumberKind
    | Array
    | Unit

type MemberDecl =
    {
        Name: string
        //Args: Ident 
        Body: Expr
    }
type ModuleDecl =
    {
        Name: string
        //Entity: EntityRef
        Members: Declaration list
    }
and Declaration =
    | ModuleDeclaration of ModuleDecl
    | MemberDeclaration of MemberDecl
