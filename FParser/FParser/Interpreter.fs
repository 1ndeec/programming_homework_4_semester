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
    | Result.Error error -> Result.Error error

    | Result.Ok program ->
        try
            let interpretExpression expression =
                expandDefinitions program.Definitions expression
                |> normalize maxSteps
                |> Result.map alphaNormalize
                |> Result.map toString

            program.Expressions
            |> List.map interpretExpression
            |> List.fold
                (fun acc result ->
                    match acc, result with
                    | Result.Error error, _ -> Result.Error error
                    | _, Result.Error error -> Result.Error error
                    | Result.Ok values, Result.Ok value -> Result.Ok(value :: values))
                (Result.Ok [])
            |> Result.map (List.rev >> String.concat System.Environment.NewLine)

        with ex ->
            Result.Error ex.Message

/// Reads a program from a file and interprets it.
let interpretFile maxSteps path =
    File.ReadAllText(path) |> interpretString maxSteps
