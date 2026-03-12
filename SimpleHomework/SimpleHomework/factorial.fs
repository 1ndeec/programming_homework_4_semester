module Factorial

let factorial x =
    let rec loop curr i x =
        if i = x then
            Some curr
        else
            loop (curr * (i + 1)) (i + 1) x

    if x < 1 then
        None
    else
        loop 1 1 x