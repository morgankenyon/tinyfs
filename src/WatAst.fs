module Fado.Core.WatAst

//type EntityPath =
//    | SourcePath of string
//    | AssemblyPath of string
//    /// Only used to reference entities in core assemblies without a path
//    | CoreAssemblyName of string
//    | PrecompiledLib of sourcePath: string * assemblyPath: string

//type EntityRef =
//    {
//        FullName: string
//        Path: EntityPath
//    }
type NumberValue =
    | Int32 of System.Int32
type ValueKind =
    | NumberConstant of value: NumberValue
type NumberKind =
    | Int32
type Expr =
    | Value of kind: ValueKind
type WatType =
    | Any
    | Number of kind: NumberKind
    | Array
type ModuleDecl =
    {
        Name: string
        //Entity: EntityRef
        Members: Declaration list
    }
and Declaration =
    | ModuleDeclaration of ModuleDecl

