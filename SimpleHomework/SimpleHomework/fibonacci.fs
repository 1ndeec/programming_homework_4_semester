let fibonacci n =
    let rec loop a b i =
        if i = n then a
        else loop b (a + b) (i + 1)

    if n < 0 then invalidArg "n" "n must be non-negative"
    elif n = 0 then 0
    else loop 0 1 1

