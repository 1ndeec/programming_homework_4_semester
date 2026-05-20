module Testwork.Tests.LambdaInterpreterTests

open NUnit.Framework
open FsUnit
open Testwork.LambdaInterpreter

let private assertAlphaEqual expected actual =
    alphaEquivalent expected actual |> should equal true

let private assertNormalizedAlphaEqual expected actual =
    match actual with
    | Some term -> assertAlphaEqual expected term
    | None -> failwith "Reduction did not finish in the given step limit."

[<Test>]
let ``identity function returns its argument`` () =
    let term =
        App(Lam("x", Var "x"), Var "y")

    term
    |> normalize 10
    |> assertNormalizedAlphaEqual (Var "y")

[<Test>]
let ``constant function returns first argument`` () =
    let term =
        App(App(Lam("x", Lam("y", Var "x")), Var "a"), Var "b")

    term
    |> normalize 10
    |> assertNormalizedAlphaEqual (Var "a")

[<Test>]
let ``normal strategy ignores divergent unused argument`` () =
    let omega =
        App(
            Lam("x", App(Var "x", Var "x")),
            Lam("x", App(Var "x", Var "x"))
        )

    let term =
        App(Lam("x", Var "y"), omega)

    term
    |> normalize 1
    |> assertNormalizedAlphaEqual (Var "y")

[<Test>]
let ``substitution avoids variable capture`` () =
    let term =
        App(Lam("x", Lam("y", Var "x")), Var "y")

    term
    |> normalize 10
    |> assertNormalizedAlphaEqual (Lam("z", Var "y"))

[<Test>]
let ``reduction works inside lambda abstraction`` () =
    let term =
        Lam("x", App(Lam("y", Var "y"), Var "x"))

    term
    |> normalize 10
    |> assertNormalizedAlphaEqual (Lam("x", Var "x"))

[<Test>]
let ``divergent term does not normalize within step limit`` () =
    let omega =
        App(
            Lam("x", App(Var "x", Var "x")),
            Lam("x", App(Var "x", Var "x"))
        )

    omega
    |> normalize 5
    |> should equal None

[<Test>]
let ``toString prints application and lambda abstraction`` () =
    let term =
        App(Lam("x", Var "x"), Var "y")

    toString term |> should equal "(\\x.x) y"