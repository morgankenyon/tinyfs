module TinyFS.Core.Helpers

open FSharp.Compiler.Symbols
open System

let isModuleValueForDeclarations (memb: FSharpMemberOrFunctionOrValue) =
    memb.CurriedParameterGroups.Count = 0 && memb.GenericParameters.Count = 0
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
        |> Option.map (fun lastParam -> hasAttrib Literals.paramList lastParam.Attributes)
        |> Option.defaultValue false

    hasParamArray memb || hasParamSeq memb

let isInline (memb: FSharpMemberOrFunctionOrValue) =
    match memb.InlineAnnotation with
    | FSharpInlineAnnotation.NeverInline
    | FSharpInlineAnnotation.OptionalInline -> false
    | FSharpInlineAnnotation.AlwaysInline
    | FSharpInlineAnnotation.AggressiveInline -> true

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

let typeIsHiddenBySignatureFile (ent: FSharpEntity) =
    not (hasOwnSignatureFile ent) && parentHasSignatureFile ent.DeclaringEntity

let isNotPrivate (memb: FSharpMemberOrFunctionOrValue) =
    if memb.IsCompilerGenerated then
        false
    elif topLevelBindingHiddenBySignatureFile memb then
        false
    else
        not memb.Accessibility.IsPrivate


module Seq =
    let mapToList (f: 'a -> 'b) (xs: 'a seq) : 'b list =
        ([], xs) ||> Seq.fold (fun li x -> (f x) :: li) |> List.rev

    let chooseToList (f: 'a -> 'b option) (xs: 'a seq) =
        ([], xs)
        ||> Seq.fold (fun li x ->
            match f x with
            | Some x -> x :: li
            | None -> li
        )
        |> List.rev