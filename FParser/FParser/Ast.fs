// Copyright (c) Murat Khamatyanov. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

module LambdaInterpreter.Ast

/// Lambda-calculus term.
type Term =
    | Var of string
    | App of Term * Term
    | Lam of string * Term

/// Named definition used by the program before the final expression.
type Definition = { Name: string; Body: Term }

/// Full input program: definitions followed by one or more expressions.
type Program =
    { Definitions: Definition list
      Expressions: Term list }
