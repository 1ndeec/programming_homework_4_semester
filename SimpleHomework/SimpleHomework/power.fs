// Copyright (c) Murat Khamatyanov. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

module SimpleHomework.Power

/// <summary>
/// Creates a list of powers of 2 from 2^n up to 2^m.
/// Throws an ArgumentException if m is greater than n.
/// </summary>
let powerList n m =
    let rec loop acc curr i m =
        if i = m then
            acc
        else
            loop (acc @ [ curr * 2 ]) (curr * 2) (i + 1) m

    if n > m then
        invalidArg "n" "n must be smaller than m"

    loop [ pown 2 n ] (pown 2 n) n m
