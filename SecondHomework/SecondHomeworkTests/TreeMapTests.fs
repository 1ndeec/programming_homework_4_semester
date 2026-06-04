// Copyright (c) Murat Khamatyanov. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

module SecondHomework.TreeMapTests

open NUnit.Framework
open FsUnit
open SecondHomework.TreeMap

/// <summary>
/// Tests mapping and linearization for binary trees.
/// </summary>
[<TestFixture>]
type TreeMapTests() =

    /// <summary>
    /// Checks that mapping over an empty tree returns an empty tree.
    /// </summary>
    [<Test>]
    member _.``mapTree should preserve empty tree``() =
        let tree: BinaryTree<int> = Empty
        let expected: BinaryTree<int> = Empty

        mapTree ((+) 1) tree |> should equal expected

    /// <summary>
    /// Checks that mapTree applies the function to every tree value
    /// without changing the tree shape.
    /// </summary>
    [<Test>]
    member _.``mapTree should apply function to every node``() =
        let tree = Node(Node(Empty, 1, Empty), 2, Node(Empty, 3, Empty))

        let expected = Node(Node(Empty, 2, Empty), 4, Node(Empty, 6, Empty))

        mapTree ((*) 2) tree |> should equal expected

    /// <summary>
    /// Checks prefix traversal order: root, left, right.
    /// </summary>
    [<Test>]
    member _.``linearize should support prefix order``() =
        let tree = Node(Node(Empty, 1, Empty), 2, Node(Empty, 3, Empty))

        linearize Prefix tree |> should equal [ 2; 1; 3 ]

    /// <summary>
    /// Checks infix traversal order: left, root, right.
    /// </summary>
    [<Test>]
    member _.``linearize should support infix order``() =
        let tree = Node(Node(Empty, 1, Empty), 2, Node(Empty, 3, Empty))

        linearize Infix tree |> should equal [ 1; 2; 3 ]

    /// <summary>
    /// Checks postfix traversal order: left, right, root.
    /// </summary>
    [<Test>]
    member _.``linearize should support postfix order``() =
        let tree = Node(Node(Empty, 1, Empty), 2, Node(Empty, 3, Empty))

        linearize Postfix tree |> should equal [ 1; 3; 2 ]
