// Copyright (c) Murat Khamatyanov. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

module SimpleHomework.First

/// <summary>
/// Finds the zero-based index of the first occurrence of a value in a list.
/// Returns None if the value is not found.
/// </summary>
let indexOfFirst x xs =
    let rec loop i xs =
        match xs with
        | [] -> None
        | h :: t -> if h = x then Some i else loop (i + 1) t

    loop 0 xs
