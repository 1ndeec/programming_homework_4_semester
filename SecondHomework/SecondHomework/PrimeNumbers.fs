// Copyright (c) Murat Khamatyanov. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

module SecondHomework.PrimeNumbers

/// <summary>
/// Checks whether the given integer is a prime number.
/// </summary>
let isPrime number =
    number > 1
    && seq { 2 .. int (sqrt (float number)) }
       |> Seq.forall (fun divisor -> number % divisor <> 0)

/// <summary>
/// Generates an infinite lazy sequence of prime numbers.
/// </summary>
let primeNumbers () =
    Seq.initInfinite id |> Seq.filter isPrime
