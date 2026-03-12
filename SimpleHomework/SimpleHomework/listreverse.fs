module Listreverse

let reverse xs =
    let rec loop acc xs =
        match xs with
        | [] -> acc
        | h :: t -> loop (h :: acc) t
    loop [] xs