module Testwork.Supermap

/// <summary>
/// Applies the function to each element of the list and concatenates all resulting lists into one.
/// </summary>
let rec supermap list f =
    match list with
    | [] -> []
    | x :: xs -> f x @ supermap xs f