module Fado.Core.Naming

let lowerFirst (s: string) =
    s.Substring(0, 1).ToLowerInvariant() + s.Substring(1)

