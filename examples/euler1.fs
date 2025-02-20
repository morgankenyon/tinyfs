module Examples

///Will clean this example up after supporting boolean logic operators
///Currently don't have an 'or' operator
let main () =
    let mutable count = 0
    let mutable n = 1

    while n < 1000 do
        let mutable num = if n % 3 = 0 then n else 0

        num <-
            if num = 0 then
                if n % 5 = 0 then n else 0
            else
                num

        count <- count + num
        n <- n + 1

    count
