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
let rec linearize order expression =
    match expression with
    | Number value -> [ string value ]
    | BinaryOperation(operator, left, right) ->
        let operatorText =
            match operator with
            | Add -> "+"
            | Subtract -> "-"
            | Multiply -> "*"
            | Divide -> "/"

        match order with
        | Prefix -> [ operatorText ] @ linearize order left @ linearize order right
        | Infix -> linearize order left @ [ operatorText ] @ linearize order right
        | Postfix -> linearize order left @ linearize order right @ [ operatorText ]
