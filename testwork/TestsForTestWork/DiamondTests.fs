module TestsForTestWork.DiamondTests

open NUnit.Framework
open FsUnit
open Testwork.Diamond

[<Test>]
let ``diamond of 1 is a single star`` () =
    diamond 1
    |> should equal "*"

[<Test>]
let ``diamond of 2 has correct shape`` () =
    diamond 2
    |> should equal " *\n***\n *"

[<Test>]
let ``diamond of 4 has correct shape`` () =
    diamond 4
    |> should equal "   *\n  ***\n *****\n*******\n *****\n  ***\n   *"

[<Test>]
let ``diamond of 3 has 5 lines`` () =
    diamond 3
    |> fun s -> s.Split('\n') |> Array.length
    |> should equal 5

[<Test>]
let ``diamond of 4 is symmetric by lines`` () =
    let lines = diamond 4 |> fun s -> s.Split('\n') |> Array.toList
    lines |> should equal ["   *"; "  ***"; " *****"; "*******"; " *****"; "  ***"; "   *"]

[<Test>]
let ``middle line of diamond of 5 is widest`` () =
    let lines = diamond 5 |> fun s -> s.Split('\n')
    lines.[4] |> should equal "*********"

[<Test>]
let ``diamond contains only spaces stars and newlines`` () =
    diamond 4
    |> Seq.forall (fun c -> c = ' ' || c = '*' || c = '\n')
    |> should equal true