// Copyright (c) Murat Khamatyanov. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

module SecondHomework.EvenNumbersTests

open NUnit.Framework
open FsUnit
open FsCheck
open SecondHomework.EvenNumbers

/// <summary>
/// Tests implementations that count even numbers in a list.
/// </summary>
[<TestFixture>]
type EvenNumbersTests() =

    /// <summary>
    /// Checks a simple list with both even and odd numbers.
    /// </summary>
    [<Test>]
    member _.``countEvenFilter should count even numbers``() =
        countEvenFilter [ 1; 2; 3; 4; 5; 6 ] |> should equal 3

    /// <summary>
    /// Checks that all three implementations are equivalent
    /// for randomly generated integer lists.
    /// </summary>
    [<Test>]
    member _.``all implementations should be equivalent``() =
        let property (numbers: int list) =
            let filterResult = countEvenFilter numbers
            let mapResult = countEvenMap numbers
            let foldResult = countEvenFold numbers

            filterResult = mapResult && mapResult = foldResult

        Check.QuickThrowOnFailure property
