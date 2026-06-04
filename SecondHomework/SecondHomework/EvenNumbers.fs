// Copyright (c) Murat Khamatyanov. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

module SecondHomework.EvenNumbers

/// <summary>
/// Counts even numbers in a list by filtering only even values
/// and then taking the length of the resulting list.
/// </summary>
let countEvenFilter numbers =
    numbers |> List.filter (fun x -> x % 2 = 0) |> List.length

/// <summary>
/// Counts even numbers in a list by mapping every even number to 1,
/// every odd number to 0, and then summing the result.
/// </summary>
let countEvenMap numbers =
    numbers |> List.map (fun x -> if x % 2 = 0 then 1 else 0) |> List.sum

/// <summary>
/// Counts even numbers in a list by folding through the list
/// and increasing the accumulator whenever an even number is found.
/// </summary>
let countEvenFold numbers =
    numbers |> List.fold (fun count x -> if x % 2 = 0 then count + 1 else count) 0
