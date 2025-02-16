module TinyFS.Core.Utils

let convertInt (o: obj) =
    match System.Int32.TryParse(o.ToString()) with
    | true, int -> Some int
    | _ -> None
