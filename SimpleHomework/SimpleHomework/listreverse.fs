// Copyright (c) Murat Khamatyanov. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

module SimpleHomework.Listreverse

/// <summary>
/// Reverses the order of elements in a list.
/// </summary>
let reverse xs =
    let rec loop acc xs =
        match xs with
        | [] -> acc
        | h :: t -> loop (h :: acc) t

    loop [] xs
