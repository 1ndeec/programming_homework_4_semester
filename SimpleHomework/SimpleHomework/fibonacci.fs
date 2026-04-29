// Copyright (c) Murat Khamatyanov. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

module SimpleHomework.Fibonacci

/// <summary>
/// Calculates the n-th Fibonacci number.
/// Throws an ArgumentException if n is negative.
/// </summary>
let fibonacci n =
    let rec loop a b i =
        if i = n then a else loop b (a + b) (i + 1)

    if n < 0 then invalidArg "n" "n must be non-negative"
    else loop 0 1 0
