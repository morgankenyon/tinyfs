module TinyFS.Core.Utils

open System
open FSharp.Compiler.Symbols

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

let convertSByte (o: obj) =
    match System.SByte.TryParse(o.ToString()) with
    | true, sb -> Some sb
    | _ -> None

let convertInt16 (o: obj) =
    match System.Int16.TryParse(o.ToString()) with
    | true, sb -> Some sb
    | _ -> None

let convertInt (o: obj) =
    match System.Int32.TryParse(o.ToString()) with
    | true, int -> Some int
    | _ -> None

let convertInt64 (o: obj) =
    match System.Int64.TryParse(o.ToString()) with
    | true, int -> Some int
    | _ -> None

let convertFloat32 (o: obj) =
    match System.Single.TryParse(o.ToString()) with
    | true, flt -> Some flt
    | _ -> None

let convertFloat64 (o: obj) =
    match System.Double.TryParse(o.ToString()) with
    | true, flt -> Some flt
    | _ -> None

let convertBool (o: obj) =
    match System.Boolean.TryParse(o.ToString()) with
    | true, bol -> Some bol
    | _ -> None

let determineExprPattern (expr) =
    match expr with
    | FSharpExprPatterns.AddressOf (a1) -> "AddressOf"
    | FSharpExprPatterns.AddressSet (a1, a2) -> "AddressSet"
    | FSharpExprPatterns.AnonRecordGet (p1, p2, p3) -> "AnonRecordGet"
    | FSharpExprPatterns.Application (p1) -> "Application"
    | FSharpExprPatterns.BaseValue (p1) -> "BaseValue"
    | FSharpExprPatterns.CallWithWitnesses (p1) -> "CallWithWitnesses"
    | FSharpExprPatterns.Call (p1) -> "Call"
    | FSharpExprPatterns.Coerce (p1) -> "Coerce"
    | FSharpExprPatterns.Const (p1) -> "Const"
    | FSharpExprPatterns.DebugPoint (p1) -> "DebugPoint"
    | FSharpExprPatterns.DecisionTreeSuccess (p1) -> "DecisionTreeSuccess"
    | FSharpExprPatterns.DecisionTree (p1) -> "DecisionTree"
    | FSharpExprPatterns.DefaultValue (p1) -> "DefaultValue"
    | FSharpExprPatterns.FSharpFieldGet (p1) -> "FSharpFieldGet"
    | FSharpExprPatterns.FSharpFieldSet (p1) -> "FSharpFieldSet"
    | FSharpExprPatterns.FastIntegerForLoop (p1) -> "FastIntegerForLoop"
    | FSharpExprPatterns.ILAsm (p1) -> "ILAsm"
    | FSharpExprPatterns.ILFieldGet (p1) -> "ILFieldGet"
    | FSharpExprPatterns.ILFieldSet (p1) -> "ILFieldSet"
    | FSharpExprPatterns.IfThenElse (p1) -> "IfThenElse"
    | FSharpExprPatterns.Lambda (p1) -> "Lambda"
    | FSharpExprPatterns.LetRec (p1) -> "LetRec"
    | FSharpExprPatterns.Let (p1) -> "Let"
    | FSharpExprPatterns.NewAnonRecord (p1) -> "NewAnonRecord"
    | FSharpExprPatterns.NewArray (p1) -> "NewArray"
    | FSharpExprPatterns.NewDelegate (p1) -> "NewDelegate"
    | FSharpExprPatterns.NewObject (p1) -> "NewObject"
    | FSharpExprPatterns.NewRecord (p1) -> "NewRecord"
    | FSharpExprPatterns.NewTuple (p1) -> "NewTuple"
    | FSharpExprPatterns.NewUnionCase (p1) -> "NewUnionCase"
    | FSharpExprPatterns.ObjectExpr (p1) -> "ObjectExpr"
    | FSharpExprPatterns.Quote (p1) -> "Quote"
    | FSharpExprPatterns.Sequential (p1) -> "Sequential"
    | FSharpExprPatterns.ThisValue (p1) -> "ThisValue"
    | FSharpExprPatterns.TraitCall (p1) -> "TraitCall"
    | FSharpExprPatterns.TryFinally (p1) -> "TryFinally"
    | FSharpExprPatterns.TryWith (p1) -> "TryWith"
    | FSharpExprPatterns.TupleGet (p1) -> "TupleGet"
    | FSharpExprPatterns.TypeLambda (p1) -> "TypeLambda"
    | FSharpExprPatterns.TypeTest (p1) -> "TypeTest"
    | FSharpExprPatterns.UnionCaseGet (p1) -> "UnionCaseGet"
    | FSharpExprPatterns.UnionCaseSet (p1) -> "UnionCaseSet"
    | FSharpExprPatterns.UnionCaseTag (p1) -> "UnionCaseTag"
    | FSharpExprPatterns.UnionCaseTest (p1) -> "UnionCaseTest"
    | FSharpExprPatterns.ValueSet (p1) -> "ValueSet"
    | FSharpExprPatterns.Value (p1) -> "Value"
    | FSharpExprPatterns.WhileLoop (p1) -> "WhileLoop"
    | FSharpExprPatterns.WitnessArg (p1) -> "WitnessArg"
    | _ -> "Unknown"

type TinyFSException(msg: string) =
    inherit System.Exception(msg)

let tinyfail (msg: string) = raise (new TinyFSException(msg))


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
