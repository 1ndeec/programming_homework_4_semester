module First

let indexOfFirst x xs =
    let rec loop i xs =
        match xs with
        | [] -> None
        | h::t -> if h = x then Some i else loop (i + 1) t
    loop 0 xs