// Copyright (c) Murat Khamatyanov. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

module LambdaInterpreter.Printer

open LambdaInterpreter.Ast

/// Collects consecutive lambda parameters into one argument list.
let rec private collectLambdaArgs term =
    match term with
    | Lam(param, body) ->
        let args, finalBody = collectLambdaArgs body

        param :: args, finalBody

    | _ -> [], term

/// Formats a lambda term as a string with the needed parentheses.
let rec private format precedence term =
    match term with
    | Var name -> name

    | Lam _ ->
        let args, body = collectLambdaArgs term

        let result = "\\" + String.concat " " args + "." + format 0 body

        if precedence > 0 then $"({result})" else result

    | App(left, right) ->
        let result = format 1 left + " " + format 2 right

        if precedence > 1 then $"({result})" else result

/// Converts a lambda term to its textual representation.
let toString term = format 0 term
