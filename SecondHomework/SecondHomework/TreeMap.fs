// Copyright (c) Murat Khamatyanov. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

module SecondHomework.TreeMap

/// <summary>
/// Represents a binary tree where each node stores one value
/// and has a left and right subtree.
/// </summary>
type BinaryTree<'T> =
    | Empty
    | Node of left: BinaryTree<'T> * value: 'T * right: BinaryTree<'T>

/// <summary>
/// Represents the order in which a binary tree can be traversed.
/// </summary>
type TraversalOrder =
    | Prefix
    | Infix
    | Postfix

type LinearizationStep<'T> =
    | Finished
    | Step of 'T * (unit -> LinearizationStep<'T>)

/// <summary>
/// Applies the given function to every value in the binary tree
/// and returns a new tree with the same structure.
/// </summary>
let rec mapTree f tree =
    match tree with
    | Empty -> Empty
    | Node(left, value, right) -> Node(mapTree f left, f value, mapTree f right)

/// <summary>
/// Converts a binary tree into a list of values using the selected traversal order.
/// </summary>
let linearize order tree =
    let rec linearizeStep tree continuation =
        match tree with
        | Empty -> continuation()
        | Node(left, value, right) ->
            match order with
            | Prefix ->
                Step(
                    value,
                    fun () -> linearizeStep left (fun () -> linearizeStep right continuation)
                )
            | Infix ->
                linearizeStep
                    left
                    (fun () ->
                        Step(
                            value,
                            fun () -> linearizeStep right continuation
                        ))
            | Postfix ->
                linearizeStep
                    left
                    (fun () ->
                        linearizeStep
                            right
                            (fun () -> Step(value, continuation)))

    let rec collect acc step =
        match step with
        | Finished -> List.rev acc
        | Step(value, getNext) -> collect (value :: acc) (getNext())

    collect [] (linearizeStep tree (fun () -> Finished))
