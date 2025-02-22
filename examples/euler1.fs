module Examples

let main () =
    let mutable count = 0
    let mutable n = 1

    while n < 1000 do
        let num = if n % 3 = 0 || n % 5 = 0 then n else 0

        count <- count + num
        n <- n + 1

    count
