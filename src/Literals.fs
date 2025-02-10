module Fado.Core.Literals

[<Literal>]
let int32 = "System.Int32"

[<Literal>]
let fablePrecompile = "Fable.Precompiled"

[<Literal>]
let createEvent =
    "Microsoft.FSharp.Core.CompilerServices.RuntimeHelpers.CreateEvent"

[<Literal>]
let unit = "Microsoft.FSharp.Core.Unit"

[<Literal>]
let paramObject = "Fable.Core.ParamObjectAttribute" // typeof<Fable.Core.ParamObjectAttribute>.FullName

[<Literal>]
let paramList = "Fable.Core.ParamListAttribute" // typeof<Fable.Core.ParamListAttribute>.FullName