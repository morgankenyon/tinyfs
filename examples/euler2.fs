module Euler2

let main () =
    let mutable first = 1
    let mutable second = 2

    let mutable sum = 2

    while second <= 4_000_000 do
        let temp = first + second
        first <- second
        second <- temp

        sum <-
            if second % 2 = 0 then
                sum + second
            else
                sum

    sum
