// Copyright (c) Murat Khamatyanov. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

module GenericHomeworkTests.BraceSequenceTests

open NUnit.Framework
open FsUnit
open GenericHomework.BraceSequence

[<Test>]
let ``empty string should be correct`` () =
    isCorrectBracketSequence "" |> should equal true

[<Test>]
let ``simple round brackets should be correct`` () =
    isCorrectBracketSequence "()" |> should equal true

[<Test>]
let ``mixed brackets should be correct`` () =
    isCorrectBracketSequence "{[()]}" |> should equal true

[<Test>]
let ``text with brackets should be correct`` () =
    isCorrectBracketSequence "a + (b * [c - d])" |> should equal true

[<Test>]
let ``single opening bracket should be incorrect`` () =
    isCorrectBracketSequence "(" |> should equal false

[<Test>]
let ``single closing bracket should be incorrect`` () =
    isCorrectBracketSequence ")" |> should equal false

[<Test>]
let ``wrong nesting should be incorrect`` () =
    isCorrectBracketSequence "{[(])}" |> should equal false

[<Test>]
let ``crossed brackets should be incorrect`` () =
    isCorrectBracketSequence "([)]" |> should equal false

[<Test>]
let ``not closed sequence should be incorrect`` () =
    isCorrectBracketSequence "(()" |> should equal false

[<Test>]
let ``extra closing bracket should be incorrect`` () =
    isCorrectBracketSequence "())" |> should equal false