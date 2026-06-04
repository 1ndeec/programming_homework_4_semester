// Copyright (c) Murat Khamatyanov. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

module SecondHomework.ParseTree

/// <summary>
/// Represents a binary arithmetic operation.
/// </summary>
type BinaryOperator =
    | Add
    | Subtract
    | Multiply
    | Divide

/// <summary>
/// Represents an arithmetic expression parse tree.
/// </summary>
type Expression =
    | Number of int
    | BinaryOperation of BinaryOperator * Expression * Expression

/// <summary>
/// Represents the order in which an expression tree can be traversed.
/// </summary>
type TraversalOrder =
    | Prefix
    | Infix
    | Postfix

type LinearizationStep =
    | Finished
    | Step of string * (unit -> LinearizationStep)

/// <summary>
/// Calculates the value of an arithmetic expression parse tree.
/// </summary>
let rec evaluate expression =
    match expression with
    | Number value -> value
    | BinaryOperation(operator, left, right) ->
        let leftValue = evaluate left
        let rightValue = evaluate right

        match operator with
        | Add -> leftValue + rightValue
        | Subtract -> leftValue - rightValue
        | Multiply -> leftValue * rightValue
        | Divide -> leftValue / rightValue

/// <summary>
/// Converts an expression parse tree into a list of string tokens
/// using the selected traversal order.
/// </summary>
let linearize order expression =
    let operatorToString operator =
        match operator with
        | Add -> "+"
        | Subtract -> "-"
        | Multiply -> "*"
        | Divide -> "/"

    let rec linearizeStep expression continuation =
        match expression with
        | Number value -> Step(string value, continuation)
        | BinaryOperation(operator, left, right) ->
            let operatorText = operatorToString operator

            match order with
            | Prefix ->
                Step(
                    operatorText,
                    fun () -> linearizeStep left (fun () -> linearizeStep right continuation)
                )
            | Infix ->
                linearizeStep
                    left
                    (fun () ->
                        Step(
                            operatorText,
                            fun () -> linearizeStep right continuation
                        ))
            | Postfix ->
                linearizeStep
                    left
                    (fun () ->
                        linearizeStep
                            right
                            (fun () -> Step(operatorText, continuation)))

    let rec collect acc step =
        match step with
        | Finished -> List.rev acc
        | Step(token, getNext) -> collect (token :: acc) (getNext())

    collect [] (linearizeStep expression (fun () -> Finished))
