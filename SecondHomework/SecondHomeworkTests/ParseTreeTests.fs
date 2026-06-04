// Copyright (c) Murat Khamatyanov. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

module SecondHomework.ParseTreeTests

open NUnit.Framework
open FsUnit
open SecondHomework.ParseTree

/// <summary>
/// Tests evaluation and linearization of arithmetic expression parse trees.
/// </summary>
[<TestFixture>]
type ParseTreeTests() =

    /// <summary>
    /// Checks that a single number evaluates to itself.
    /// </summary>
    [<Test>]
    member _.``evaluate should return number value``() = evaluate (Number 42) |> should equal 42

    /// <summary>
    /// Checks evaluation of a nested arithmetic expression.
    /// </summary>
    [<Test>]
    member _.``evaluate should calculate nested expression``() =
        let expression =
            BinaryOperation(
                Multiply,
                BinaryOperation(Add, Number 2, Number 3),
                BinaryOperation(Subtract, Number 10, Number 4)
            )

        evaluate expression |> should equal 30

    /// <summary>
    /// Checks that integer division is evaluated correctly.
    /// </summary>
    [<Test>]
    member _.``evaluate should support integer division``() =
        let expression = BinaryOperation(Divide, Number 10, Number 2)

        evaluate expression |> should equal 5

    /// <summary>
    /// Checks prefix linearization of an arithmetic expression.
    /// </summary>
    [<Test>]
    member _.``linearize should support prefix order``() =
        let expression =
            BinaryOperation(
                Multiply,
                BinaryOperation(Add, Number 2, Number 3),
                BinaryOperation(Subtract, Number 10, Number 4)
            )

        linearize Prefix expression
        |> should equal [ "*"; "+"; "2"; "3"; "-"; "10"; "4" ]

    /// <summary>
    /// Checks infix linearization of an arithmetic expression.
    /// </summary>
    [<Test>]
    member _.``linearize should support infix order``() =
        let expression =
            BinaryOperation(
                Multiply,
                BinaryOperation(Add, Number 2, Number 3),
                BinaryOperation(Subtract, Number 10, Number 4)
            )

        linearize Infix expression
        |> should equal [ "2"; "+"; "3"; "*"; "10"; "-"; "4" ]

    /// <summary>
    /// Checks postfix linearization of an arithmetic expression.
    /// </summary>
    [<Test>]
    member _.``linearize should support postfix order``() =
        let expression =
            BinaryOperation(
                Multiply,
                BinaryOperation(Add, Number 2, Number 3),
                BinaryOperation(Subtract, Number 10, Number 4)
            )

        linearize Postfix expression
        |> should equal [ "2"; "3"; "+"; "10"; "4"; "-"; "*" ]
