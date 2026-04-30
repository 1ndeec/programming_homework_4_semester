module LambdaInterpreter.Tests

open System
open System.IO
open NUnit.Framework
open FsUnit

open LambdaInterpreter.Interpreter
open LambdaInterpreter.Parser

let private run (input: string) : Result<string, string> = interpretString 10000 input

let private runFile (input: string) : Result<string, string> =
    let path =
        Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N") + ".lambda")

    try
        File.WriteAllText(path, input)
        interpretFile 10000 path
    finally
        if File.Exists path then
            File.Delete path

let private ok (value: string) : Result<string, string> = Result.Ok value

[<Test>]
let ``Task example S K K should reduce to identity`` () =
    let input =
        """
let S = \x y z.x z (y z)

let K = \x y.x

S K K
"""

    run input |> should equal (ok @"\x.x")

[<Test>]
let ``Identity function should return its argument`` () =
    let input = @"(\x.x) y"

    run input |> should equal (ok "y")

[<Test>]
let ``K combinator should return first argument`` () =
    let input = @"(\x y.x) a b"

    run input |> should equal (ok "a")

[<Test>]
let ``Application should be left associative`` () =
    let input =
        """
let I = \x.x

I a b
"""

    run input |> should equal (ok "a b")

[<Test>]
let ``Parser should reject let as variable name`` () =
    let input = @"\let.let"

    match parseTerm input with
    | Result.Ok term -> Assert.Fail $"Parser unexpectedly accepted term: {term}"

    | Result.Error _ -> Assert.Pass()

[<Test>]
let ``Duplicate definitions should be rejected`` () =
    let input =
        """
let I = \x.x
let I = \y.y

I
"""

    match run input with
    | Result.Ok result -> Assert.Fail $"Interpreter unexpectedly accepted duplicate definitions: {result}"

    | Result.Error error -> error |> should haveSubstring "Duplicate definition"

[<Test>]
let ``Alpha conversion should prevent variable capture`` () =
    let input = @"(\x.\y.x) y"

    run input |> should equal (ok @"\x.y")

[<Test>]
let ``File input should reduce task example`` () =
    let input =
        """
let S = \x y z.x z (y z)

let K = \x y.x

S K K
"""

    runFile input |> should equal (ok @"\x.x")

[<Test>]
let ``File input should reduce identity application`` () =
    let input =
        """
let I = \x.x

I q
"""

    runFile input |> should equal (ok "q")
