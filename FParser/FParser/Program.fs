// Copyright (c) Murat Khamatyanov. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

module Program

open System
open System.IO
open LambdaInterpreter.Interpreter

[<EntryPoint>]
let main args =
    let maxSteps = 10000

    let input =
        match Array.toList args with
        | [] ->
            Console.WriteLine "Lambda interpreter"
            Console.WriteLine "Enter lambda expressions and optional definitions."
            Console.WriteLine @"Example:"
            Console.WriteLine @"let I = \x.x"
            Console.WriteLine @"I a"
            Console.WriteLine "Finish input with ^Z in the next line after the input text body, then Enter."
            Console.WriteLine()
            Console.In.ReadToEnd()

        | [ path ] when File.Exists path -> File.ReadAllText path
        | parts -> String.concat " " parts

    match interpretString maxSteps input with
    | Result.Ok output ->
        Console.WriteLine output
        0

    | Result.Error error ->
        Console.Error.WriteLine error
        1
