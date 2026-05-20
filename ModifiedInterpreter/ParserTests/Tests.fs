// Copyright (c) Murat Khamatyanov. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

module Testwork.Tests.LambdaParserTests

open NUnit.Framework
open FsUnit
open Testwork.LambdaInterpreter
open Testwork.LambdaParser

let private shouldParseAs expected input =
    match parseTerm input with
    | Ok actual -> actual |> should equal expected
    | Error error -> failwithf "Expected successful parse, but got error: %s" error

let private shouldFailToParse input =
    match parseTerm input with
    | Ok term -> failwithf "Expected parse error, but parsed term: %A" term
    | Error _ -> ()

let private shouldInterpretAs expected input =
    match interpretString 100 input with
    | Ok actual -> actual |> should equal expected
    | Error error -> failwithf "Expected successful interpretation, but got error: %s" error

[<Test>]
let ``parse variable`` () = "x" |> shouldParseAs (Var "x")

[<Test>]
let ``parse simple lambda abstraction`` () =
    "\\x.x" |> shouldParseAs (Lam("x", Var "x"))

[<Test>]
let ``parse lambda abstraction with several parameters`` () =
    "\\x y z.x" |> shouldParseAs (Lam("x", Lam("y", Lam("z", Var "x"))))

[<Test>]
let ``parse application as left associative`` () =
    "x y z" |> shouldParseAs (App(App(Var "x", Var "y"), Var "z"))

[<Test>]
let ``parse parentheses in application`` () =
    "x (y z)" |> shouldParseAs (App(Var "x", App(Var "y", Var "z")))

[<Test>]
let ``parse lambda body containing application`` () =
    "\\x y.x y" |> shouldParseAs (Lam("x", Lam("y", App(Var "x", Var "y"))))

[<Test>]
let ``parse S combinator`` () =
    "\\x y z.x z (y z)"
    |> shouldParseAs (Lam("x", Lam("y", Lam("z", App(App(Var "x", Var "z"), App(Var "y", Var "z"))))))

[<Test>]
let ``parse program with one definition`` () =
    let input = "let I = \\x.x\nI y"

    match parseProgram input with
    | Error error -> failwithf "Expected successful program parse, but got error: %s" error

    | Ok program ->
        program.Definitions.Length |> should equal 1
        program.Definitions.Head.Name |> should equal "I"
        program.Definitions.Head.Body |> should equal (Lam("x", Var "x"))
        program.Expression |> should equal (App(Var "I", Var "y"))

[<Test>]
let ``expand simple definition`` () =
    let input = "let I = \\x.x\nI y"

    match parseProgram input with
    | Error error -> failwithf "Expected successful program parse, but got error: %s" error

    | Ok program -> expandDefinitions program |> should equal (App(Lam("x", Var "x"), Var "y"))

[<Test>]
let ``interpret identity definition`` () =
    let input = "let I = \\x.x\nI y"

    input |> shouldInterpretAs "y"

[<Test>]
let ``interpret S K K as identity`` () =
    let input = "let S = \\x y z.x z (y z)\nlet K = \\x y.x\nS K K"

    input |> shouldInterpretAs "\\__v0.__v0"

[<Test>]
let ``interpret S K K applied to argument`` () =
    let input = "let S = \\x y z.x z (y z)\nlet K = \\x y.x\nS K K a"

    input |> shouldInterpretAs "a"

[<Test>]
let ``keyword let cannot be parsed as variable`` () = "let" |> shouldFailToParse

[<Test>]
let ``lambda without parameter fails`` () = "\\.x" |> shouldFailToParse

[<Test>]
let ``unclosed parenthesis fails`` () = "(x y" |> shouldFailToParse

[<Test>]
let ``definition after final expression fails`` () =
    let input = "x\nlet I = \\x.x"

    match parseProgram input with
    | Ok _ -> failwith "Expected parse error."
    | Error error -> error |> should equal "Definitions must appear before the final expression."

[<Test>]
let ``duplicate definitions fail during interpretation`` () =
    let input = "let I = \\x.x\nlet I = \\y.y\nI"

    match interpretString 100 input with
    | Ok result -> failwithf "Expected duplicate definition error, but got result: %s" result

    | Error error -> Assert.That(error, Does.Contain("Duplicate definition"))

[<Test>]
let ``cyclic definitions fail during interpretation`` () =
    let input = "let A = B\nlet B = A\nA"

    match interpretString 100 input with
    | Ok result -> failwithf "Expected cyclic definition error, but got result: %s" result

    | Error error -> Assert.That(error, Does.Contain("Cyclic definition"))
