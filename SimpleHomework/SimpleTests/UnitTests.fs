// Copyright (c) Murat Khamatyanov. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

module SimpleHomework.Tests

open System
open NUnit.Framework
open FsUnit

open SimpleHomework.Factorial
open SimpleHomework.Power
open SimpleHomework.First
open SimpleHomework.Listreverse
open SimpleHomework.Fibonacci

module FactorialTests =

    [<Test>]
    let ``factorial of positive number should return Some factorial`` () =
        factorial 5 |> should equal (Some 120)

    [<Test>]
    let ``factorial of one should return Some one`` () =
        factorial 1 |> should equal (Some 1)

    [<Test>]
    let ``factorial of zero should return None`` () =
        factorial 0 |> should equal None

    [<Test>]
    let ``factorial of negative number should return None`` () =
        factorial -3 |> should equal None


module PowerListTests =

    [<Test>]
    let ``powerList should return powers of two from n to m`` () =
        powerList 3 5 |> should equal [8; 16; 32]

    [<Test>]
    let ``powerList should work when n equals m`` () =
        powerList 4 4 |> should equal [16]

    [<Test>]
    let ``powerList should throw when n is greater than m`` () =
        (fun () -> powerList 5 3 |> ignore)
        |> should throw typeof<ArgumentException>


module IndexOfFirstTests =

    [<Test>]
    let ``indexOfFirst should return index of first occurrence`` () =
        indexOfFirst 3 [1; 2; 3; 4] |> should equal (Some 2)

    [<Test>]
    let ``indexOfFirst should return first index when value appears several times`` () =
        indexOfFirst 2 [1; 2; 3; 2; 4] |> should equal (Some 1)

    [<Test>]
    let ``indexOfFirst should return None when value is absent`` () =
        indexOfFirst 5 [1; 2; 3] |> should equal None

    [<Test>]
    let ``indexOfFirst should return None for empty list`` () =
        indexOfFirst 1 [] |> should equal None


module ReverseTests =

    [<Test>]
    let ``reverse should reverse non-empty list`` () =
        reverse [1; 2; 3; 4] |> should equal [4; 3; 2; 1]

    [<Test>]
    let ``reverse should return empty list for empty list`` () =
        reverse [] |> should equal []

    [<Test>]
    let ``reverse should work with one element`` () =
        reverse [42] |> should equal [42]


module FibonacciTests =

    [<Test>]
    let ``fibonacci of zero should return Some zero`` () =
        fibonacci 0 |> should equal (Some 0)

    [<Test>]
    let ``fibonacci of one should return Some one`` () =
        fibonacci 1 |> should equal (Some 1)

    [<Test>]
    let ``fibonacci of six should return Some eight`` () =
        fibonacci 6 |> should equal (Some 8)

    [<Test>]
    let ``fibonacci should return None when n is negative`` () =
        fibonacci -1 |> should equal None
