// Copyright (c) Murat Khamatyanov. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

module Testwork.Workflows

open System

/// <summary>
/// Workflow builder that performs integer calculations over values given as strings.
/// Each bound string is parsed as an integer; if parsing fails, the whole workflow returns None.
/// </summary>
type CalculateBuilder() =
    member _.Bind(value: string, continuation: int -> int option) =
        match Int32.TryParse(value) with
        | true, number -> continuation number
        | false, _ -> None

    member _.Return(value: int) = Some value

/// <summary>
/// Creates a calculation workflow builder for integer values written as strings.
/// </summary>
let calculate () = CalculateBuilder()


/// <summary>
/// Workflow builder that performs floating-point calculations and rounds intermediate
/// bound values and the final result to the specified number of decimal digits.
/// </summary>
type RoundingBuilder(precision: int) =
    let round (value: float) = Math.Round(value, precision)

    member _.Bind(value: float, continuation: float -> float) = continuation (round value)

    member _.Return(value: float) = round value

/// <summary>
/// Creates a rounding workflow builder with the given decimal precision.
/// </summary>
let rounding precision =
    RoundingBuilder(precision)
