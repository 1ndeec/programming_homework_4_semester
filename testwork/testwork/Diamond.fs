// Copyright (c) Murat Khamatyanov. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

module Testwork.Diamond

/// <summary>
/// Builds a string representation of a diamond made of '*' characters with side length n.
/// </summary>
let diamond n =
    let line i =
        String.replicate (n - i) " " +
        String.replicate (2 * i - 1) "*"

    [1 .. n]
    @ [n - 1 .. -1 .. 1]
    |> List.map line
    |> String.concat "\n"