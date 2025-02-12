module TinyFS.Core.Transforms

open FSharp.Compiler.Symbols
open System

let rec nonAbbreviatedDefinition (ent: FSharpEntity) : FSharpEntity =
    if ent.IsFSharpAbbreviation then
        let t = ent.AbbreviatedType

        if t.HasTypeDefinition && t.TypeDefinition <> ent then
            nonAbbreviatedDefinition t.TypeDefinition
        else
            ent
    else
        ent
let ensureFsExtension (path: string) =
    if path.EndsWith(".fsi", StringComparison.Ordinal) then
        path.Substring(0, path.Length - 1)
    else
        path
let normalizePath (path: string) = path.Replace('\\', '/').TrimEnd('/')
let normalizePathAndEnsureFsExtension (path: string) = normalizePath path |> ensureFsExtension
let tryArrayFullName (ent: FSharpEntity) =
    if ent.IsArrayType then
        let rank =
            match ent.ArrayRank with
            | rank when rank > 1 -> "`" + string<int> rank
            | _ -> ""

        Some("System.Array" + rank)
    else
        None

let tryFindAttrib fullName (atts: FSharpAttribute seq) =
    atts
    |> Seq.tryPick (fun att ->
        match (nonAbbreviatedDefinition att.AttributeType).TryFullName with
        | Some fullName2 ->
            if fullName = fullName2 then
                Some att
            else
                None
        | None -> None
    )

let SourcePath(ent: FSharpEntity) =
    let ent = nonAbbreviatedDefinition ent

    ent.DeclarationLocation.FileName |> normalizePathAndEnsureFsExtension

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

module Patterns =
    let (|DicContains|_|) (dic: System.Collections.Generic.IDictionary<'k, 'v>) key =
        let success, value = dic.TryGetValue key

        if success then
            Some value
        else
            None
    let (|SetContains|_|) set item =
        if Set.contains item set then
            Some SetContains
        else
            None
module Operators =
    [<Literal>]
    let addition = "op_Addition"

    [<Literal>]
    let subtraction = "op_Subtraction"

    [<Literal>]
    let multiply = "op_Multiply"

    [<Literal>]
    let division = "op_Division"

    [<Literal>]
    let modulus = "op_Modulus"

    [<Literal>]
    let leftShift = "op_LeftShift"

    [<Literal>]
    let rightShift = "op_RightShift"

    [<Literal>]
    let bitwiseAnd = "op_BitwiseAnd"

    [<Literal>]
    let bitwiseOr = "op_BitwiseOr"

    [<Literal>]
    let exclusiveOr = "op_ExclusiveOr"

    [<Literal>]
    let booleanAnd = "op_BooleanAnd"

    [<Literal>]
    let booleanOr = "op_BooleanOr"

    [<Literal>]
    let logicalNot = "op_LogicalNot"

    [<Literal>]
    let unaryNegation = "op_UnaryNegation"

    [<Literal>]
    let unaryPlus = "op_UnaryPlus"

    [<Literal>]
    let divideByInt = "DivideByInt"

    [<Literal>]
    let equality = "op_Equality"

    [<Literal>]
    let inequality = "op_Inequality"

    [<Literal>]
    let lessThan = "op_LessThan"

    [<Literal>]
    let greaterThan = "op_GreaterThan"

    [<Literal>]
    let lessThanOrEqual = "op_LessThanOrEqual"

    [<Literal>]
    let greaterThanOrEqual = "op_GreaterThanOrEqual"

    let standardSet =
        set
            [
                addition
                subtraction
                multiply
                division
                modulus
                leftShift
                rightShift
                bitwiseAnd
                bitwiseOr
                exclusiveOr
                booleanAnd
                booleanOr
                logicalNot
                unaryNegation
                unaryPlus
            ]

    let compareSet =
        set
            [
                equality
                "Eq"
                inequality
                "Neq"
                lessThan
                "Lt"
                lessThanOrEqual
                "Lte"
                greaterThan
                "Gt"
                greaterThanOrEqual
                "Gte"
            ]