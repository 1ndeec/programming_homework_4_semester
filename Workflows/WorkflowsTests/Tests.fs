module Testwork.Tests.WorkflowsTests

open NUnit.Framework
open FsUnit
open Testwork.Workflows

[<Test>]
let ``calculate returns Some 3 for valid numeric strings`` () =
    let result =
        calculate () {
            let! x = "1"
            let! y = "2"
            let z = x + y
            return z
        }

    result |> should equal (Some 3)


[<Test>]
let ``calculate returns None when second string is not a number`` () =
    let result =
        calculate () {
            let! x = "1"
            let! y = "b"
            let z = x + y
            return z
        }

    result |> should equal None


[<Test>]
let ``calculate returns None when first string is not a number`` () =
    let result =
        calculate () {
            let! x = "hello"
            let! y = "2"
            return x + y
        }

    result |> should equal None


[<Test>]
let ``calculate works with negative numbers`` () =
    let result =
        calculate () {
            let! x = "-10"
            let! y = "4"
            let z = x + y
            return z
        }

    result |> should equal (Some -6)


[<Test>]
let ``rounding returns rounded result with precision 3`` () =
    let result =
        rounding 3 {
            let! a = 2.0 / 12.0
            let! b = 3.5
            return a / b
        }

    result |> should (equalWithin 0.000001) 0.048


[<Test>]
let ``rounding rounds let bang value`` () =
    let result =
        rounding 2 {
            let! x = 1.0 / 3.0
            return x
        }

    result |> should (equalWithin 0.000001) 0.33


[<Test>]
let ``rounding rounds final return value`` () =
    let result =
        rounding 1 {
            let! x = 10.0
            let! y = 4.0
            return x / y
        }

    result |> should (equalWithin 0.000001) 2.5
