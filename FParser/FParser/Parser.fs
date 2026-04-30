// Copyright (c) Murat Khamatyanov. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

module LambdaInterpreter.Parser

open System
open FParsec
open LambdaInterpreter.Ast

/// Short alias for an FParsec parser with no custom user state.
type private P<'T> = Parser<'T, unit>

/// Parser that consumes any amount of whitespace.
let private ws: P<unit> = spaces

/// Adds trailing whitespace consumption to a parser.
let private lexeme parser = parser .>> ws

/// Parses a fixed string and consumes trailing whitespace.
let private symbol text = lexeme (pstring text)

/// Parses a fixed character and consumes trailing whitespace.
let private ch c = lexeme (pchar c)

/// Checks whether a character can start an identifier.
let private isIdentStart c = Char.IsLetter(c) || c = '_'

/// Checks whether a character can continue an identifier.
let private isIdentContinue c =
    Char.IsLetterOrDigit(c) || c = '_' || c = '\''

/// Parses an identifier, except for the reserved keyword "let".
let identifier: P<string> =
    // Parser for the raw identifier text.
    let raw = many1Satisfy2L isIdentStart isIdentContinue "identifier"

    lexeme raw
    >>= fun name ->
        if name = "let" then
            fail "keyword 'let' cannot be used as an identifier"
        else
            preturn name

/// Forward-declared parser for lambda terms.
let term, termRef = createParserForwardedToRef<Term, unit> ()

/// Parses the beginning of a lambda abstraction.
let private lambdaStart: P<char> = lexeme (pchar '\\' <|> pchar 'λ')

/// Parses a variable term.
let private variable: P<Term> = identifier |>> Var

/// Parses a term inside parentheses.
let private parenthesized: P<Term> = between (ch '(') (ch ')') term

/// Parses the smallest indivisible term form.
let private atom: P<Term> = parenthesized <|> variable

/// Builds a left-associative application tree from parsed atoms.
let private makeApplication atoms =
    match atoms with
    | [] -> failwith "Internal parser error: empty application."
    | head :: tail -> List.fold (fun acc arg -> App(acc, arg)) head tail

/// Parses one or more atoms as a left-associative application.
let private application: P<Term> = many1 atom |>> makeApplication

/// Builds nested lambda abstractions from a list of parameters.
let private makeLambda (args, body) =
    args |> List.rev |> List.fold (fun acc arg -> Lam(arg, acc)) body

/// Parses a lambda abstraction with one or more parameters.
let private abstraction: P<Term> =
    lambdaStart >>. many1 identifier .>> ch '.' .>>. term |>> makeLambda

/// Defines the recursive term parser.
do termRef := abstraction <|> application

/// Parses a complete lambda term and requires end of input.
let private fullTerm: P<Term> = ws >>. term .>> eof

/// Parses one named definition line.
let private definitionLine: P<Definition> =
    ws >>. pstring "let" >>. spaces1 >>. identifier .>> symbol "=" .>>. term .>> eof
    |>> fun (name, body) -> { Name = name; Body = body }

/// Runs an FParsec parser and converts the result to F# Result.
let private runParser<'T> (parser: Parser<'T, unit>) (input: string) : Result<'T, string> =
    match run parser input with
    | Success(result, _, _) -> Result.Ok result
    | Failure(error, _, _) -> Result.Error error

/// Parses a single lambda term from a string.
let parseTerm (input: string) : Result<Term, string> = runParser fullTerm input

/// Converts all newline formats to Unix-style newlines.
let private normalizeNewLines (text: string) =
    text.Replace("\r\n", "\n").Replace("\r", "\n")

/// Checks whether a line starts with a definition.
let private isLetLine (line: string) = line.TrimStart().StartsWith("let ")

/// Parses a full program with definitions followed by a final expression.
let parseProgram (input: string) : Result<Program, string> =
    // Non-empty normalized input lines.
    let lines =
        (normalizeNewLines input).Split('\n')
        |> Array.map (fun line -> line.Trim())
        |> Array.filter (fun line -> not (System.String.IsNullOrWhiteSpace line))
        |> Array.toList

    // Collects all initial let-definitions before the final expression.
    let rec collectDefinitions acc rest =
        match rest with
        | [] -> Result.Error "Expected final lambda expression after definitions."

        | line :: tail when isLetLine line ->
            match runParser definitionLine line with
            | Result.Ok definition -> collectDefinitions (definition :: acc) tail

            | Result.Error error -> Result.Error $"Invalid definition line: {line}\n{error}"

        | _ -> Result.Ok(List.rev acc, rest)

    match collectDefinitions [] lines with
    | Result.Error error -> Result.Error error

    | Result.Ok(definitions, expressionLines) ->
        match expressionLines |> List.tryFind isLetLine with
        | Some badLine -> Result.Error $"Definitions must appear before the final expression. Bad line: {badLine}"

        | None ->
            let duplicate =
                definitions
                |> List.countBy (fun definition -> definition.Name)
                |> List.tryFind (fun (_, count) -> count > 1)

            match duplicate with
            | Some(name, _) -> Result.Error $"Duplicate definition: {name}"

            | None ->
                let expressionText = String.concat " " expressionLines

                match parseTerm expressionText with
                | Result.Ok expression ->
                    Result.Ok
                        { Definitions = definitions
                          Expression = expression }

                | Result.Error error -> Result.Error $"Invalid final expression:\n{error}"