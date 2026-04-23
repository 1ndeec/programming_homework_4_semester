// Copyright (c) Murat Khamatyanov. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

module Testwork.Supermap

/// <summary>
/// Applies the function to each element of the list and concatenates all resulting lists into one.
/// </summary>
let rec supermap list f =
    match list with
    | [] -> []
    | x :: xs -> f x @ supermap xs f