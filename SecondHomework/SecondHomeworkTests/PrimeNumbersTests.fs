// Copyright (c) Murat Khamatyanov. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

module SecondHomework.PrimeNumbersTests

open NUnit.Framework
open FsUnit
open SecondHomework.PrimeNumbers

/// <summary>
/// Tests prime number checking and infinite prime number generation.
/// </summary>
[<TestFixture>]
type PrimeNumbersTests() =

    /// <summary>
    /// Checks that known prime numbers are detected as prime.
    /// </summary>
    [<Test>]
    member _.``isPrime should return true for prime numbers``() =
        [ 2; 3; 5; 7; 11; 13; 17; 19 ] |> List.forall isPrime |> should equal true

    /// <summary>
    /// Checks that known non-prime numbers are rejected.
    /// </summary>
    [<Test>]
    member _.``isPrime should return false for non-prime numbers``() =
        [ -5; -1; 0; 1; 4; 6; 8; 9; 10; 12 ]
        |> List.exists isPrime
        |> should equal false

    /// <summary>
    /// Checks the first ten generated prime numbers.
    /// </summary>
    [<Test>]
    member _.``primeNumbers should generate first ten primes``() =
        primeNumbers ()
        |> Seq.take 10
        |> Seq.toList
        |> should equal [ 2; 3; 5; 7; 11; 13; 17; 19; 23; 29 ]

    /// <summary>
    /// Checks that the generated sequence can be consumed beyond the first values.
    /// </summary>
    [<Test>]
    member _.``primeNumbers should generate later primes lazily``() =
        primeNumbers () |> Seq.item 20 |> should equal 73
