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
let rec linearize order tree =
    match tree with
    | Empty -> []
    | Node(left, value, right) ->
        match order with
        | Prefix -> [ value ] @ linearize order left @ linearize order right
        | Infix -> linearize order left @ [ value ] @ linearize order right
        | Postfix -> linearize order left @ linearize order right @ [ value ]
