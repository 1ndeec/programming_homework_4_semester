// Copyright (c) Murat Khamatyanov. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

module LambdaInterpreter.Interpreter

open System.IO
open LambdaInterpreter.Parser
open LambdaInterpreter.Reducer
open LambdaInterpreter.Printer

/// Parses, expands, reduces, and prints a lambda-calculus program from a string.
let interpretString maxSteps input =
    match parseProgram input with
    | Result.Error error ->
        Result.Error error

    | Result.Ok program ->
        try
            let expanded = expandDefinitions program

            expanded
            |> normalize maxSteps
            |> Result.map alphaNormalize
            |> Result.map toString

        with ex ->
            Result.Error ex.Message

/// Reads a program from a file and interprets it.
let interpretFile maxSteps path =
    File.ReadAllText(path) |> interpretString maxSteps
