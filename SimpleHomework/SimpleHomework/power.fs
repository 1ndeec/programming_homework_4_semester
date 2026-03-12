module Power

let powerList n m =
    let rec loop acc curr i m = 
        if i = m then
            acc
        else
            loop (acc @ [curr*2]) (curr*2) (i+1) m

    if m > n then invalidArg "n" "n must be smaller than m"

    loop [pown 2 n] (pown 2 n) n m