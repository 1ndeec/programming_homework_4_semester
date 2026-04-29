// Copyright (c) Murat Khamatyanov. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

module SimpleHomework.Factorial

/// <summary>
/// Calculates the factorial of a positive integer.
/// Returns None if the input is less than 1.
/// </summary>
let factorial x =
    let rec loop curr i x =
        if i = x then Some curr else loop (curr * (i + 1)) (i + 1) x

    if x < 1 then None else loop 1 1 x
