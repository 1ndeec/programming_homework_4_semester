// Copyright (c) Murat Khamatyanov. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

module Testwork.LambdaParser

open System
open System.IO
open Testwork.LambdaInterpreter

/// <summary>
/// Represents one named lambda-expression definition.
/// </summary>
type Definition = { Name: string; Body: Term }

/// <summary>
/// Represents a full input program: zero or more definitions and one final expression.
/// </summary>
type Program =
    { Definitions: Definition list
      Expression: Term }

type private Token =
    | Lambda
    | Dot
    | LeftParen
    | RightParen
    | Equals
    | Let
    | Identifier of string
    | End

exception private ParseException of string

let private tokenToString token =
    match token with
    | Lambda -> "\\"
    | Dot -> "."
    | LeftParen -> "("
    | RightParen -> ")"
    | Equals -> "="
    | Let -> "let"
    | Identifier name -> name
    | End -> "end of input"

let private isIdentifierStart character =
    Char.IsLetter(character) || character = '_'

let private isIdentifierPart character =
    Char.IsLetterOrDigit(character) || character = '_' || character = '\''

let private tokenize (input: string) =
    let rec loop position tokens =
        if position >= input.Length then
            List.rev (End :: tokens)
        else
            match input.[position] with
            | character when Char.IsWhiteSpace(character) -> loop (position + 1) tokens

            | '\\'
            | 'λ' -> loop (position + 1) (Lambda :: tokens)

            | '.' -> loop (position + 1) (Dot :: tokens)

            | '(' -> loop (position + 1) (LeftParen :: tokens)

            | ')' -> loop (position + 1) (RightParen :: tokens)

            | '=' -> loop (position + 1) (Equals :: tokens)

            | character when isIdentifierStart character ->
                let mutable endPosition = position + 1

                while endPosition < input.Length && isIdentifierPart input.[endPosition] do
                    endPosition <- endPosition + 1

                let text = input.Substring(position, endPosition - position)

                let token = if text = "let" then Let else Identifier text

                loop endPosition (token :: tokens)

            | character -> raise (ParseException(sprintf "Unexpected character '%c'." character))

    loop 0 []

type private TokenParser(tokens: Token list) =
    let tokenArray = tokens |> List.toArray
    let mutable position = 0

    member private _.Current =
        if position < tokenArray.Length then
            tokenArray.[position]
        else
            End

    member private this.Advance() =
        let current = this.Current
        position <- position + 1
        current

    member private this.Expect(expected: Token) =
        if this.Current = expected then
            this.Advance() |> ignore
        else
            raise (
                ParseException(
                    sprintf "Expected '%s', but got '%s'." (tokenToString expected) (tokenToString this.Current)
                )
            )

    member private this.ParseIdentifier() =
        match this.Current with
        | Identifier name ->
            this.Advance() |> ignore
            name

        | Let -> raise (ParseException "'let' is a keyword and cannot be used as a variable name.")

        | token -> raise (ParseException(sprintf "Expected identifier, but got '%s'." (tokenToString token)))

    member private this.StartsAtom =
        match this.Current with
        | Identifier _
        | LeftParen -> true
        | _ -> false

    member private this.ParseAtom() =
        match this.Current with
        | Identifier name ->
            this.Advance() |> ignore
            Var name

        | LeftParen ->
            this.Advance() |> ignore
            let term = this.ParseExpression()
            this.Expect RightParen
            term

        | token -> raise (ParseException(sprintf "Expected atom, but got '%s'." (tokenToString token)))

    member private this.ParseApplication() =
        if not this.StartsAtom then
            raise (ParseException(sprintf "Expected expression, but got '%s'." (tokenToString this.Current)))

        let first = this.ParseAtom()

        let rec loop current =
            if this.StartsAtom then
                let next = this.ParseAtom()
                loop (App(current, next))
            else
                current

        loop first

    member private this.ParseLambda() =
        this.Expect Lambda

        let rec collectParameters parameters =
            match this.Current with
            | Identifier _ ->
                let parameter = this.ParseIdentifier()
                collectParameters (parameter :: parameters)
            | _ -> List.rev parameters

        let parameters = collectParameters []

        if List.isEmpty parameters then
            raise (ParseException "Lambda abstraction must have at least one parameter.")

        this.Expect Dot

        let body = this.ParseExpression()

        List.foldBack (fun parameter currentBody -> Lam(parameter, currentBody)) parameters body

    member this.ParseExpression() =
        match this.Current with
        | Lambda -> this.ParseLambda()
        | _ -> this.ParseApplication()

    member this.ParseAll() =
        let result = this.ParseExpression()

        match this.Current with
        | End -> result
        | token -> raise (ParseException(sprintf "Unexpected token '%s' after expression." (tokenToString token)))

/// <summary>
/// Parses one lambda-expression without named definitions.
/// </summary>
let parseTerm input =
    try
        let parser = TokenParser(tokenize input)
        Ok(parser.ParseAll())
    with ParseException error ->
        Error error

let private validateDefinitionName name =
    match tokenize name with
    | [ Identifier parsedName; End ] when parsedName = name -> Ok parsedName

    | [ Let; End ] -> Error "'let' is a keyword and cannot be used as a definition name."

    | _ -> Error(sprintf "Invalid definition name '%s'." name)

let private parseDefinitionLine lineNumber (line: string) =
    let trimmed = line.Trim()

    if not (trimmed.StartsWith("let ")) then
        Error(sprintf "Line %d is not a definition." lineNumber)
    else
        let afterLet = trimmed.Substring(3).TrimStart()
        let equalsPosition = afterLet.IndexOf('=')

        if equalsPosition < 0 then
            Error(sprintf "Definition on line %d must contain '='." lineNumber)
        else
            let name = afterLet.Substring(0, equalsPosition).Trim()
            let bodyText = afterLet.Substring(equalsPosition + 1).Trim()

            match validateDefinitionName name with
            | Error error -> Error(sprintf "Definition error on line %d: %s" lineNumber error)

            | Ok parsedName ->
                match parseTerm bodyText with
                | Ok body -> Ok { Name = parsedName; Body = body }

                | Error error -> Error(sprintf "Definition parse error on line %d: %s" lineNumber error)

let private isDefinitionLine (line: string) = line.TrimStart().StartsWith("let ")

/// <summary>
/// Parses a program containing one-line let-definitions followed by one lambda-expression.
/// </summary>
let parseProgram (input: string) =
    let lines =
        input.Replace("\r\n", "\n").Replace("\r", "\n").Split('\n')
        |> Array.map (fun line -> line.Trim())
        |> Array.filter (fun line -> line <> "")

    let rec loop index definitions expressionLines expressionStarted =
        if index = lines.Length then
            match expressionLines with
            | [] -> Error "Expected final lambda-expression."

            | _ ->
                let expressionText = expressionLines |> List.rev |> String.concat " "

                match parseTerm expressionText with
                | Ok expression ->
                    Ok
                        { Definitions = List.rev definitions
                          Expression = expression }

                | Error error -> Error(sprintf "Final expression parse error: %s" error)
        else
            let line = lines.[index]

            if isDefinitionLine line then
                if expressionStarted then
                    Error "Definitions must appear before the final expression."
                else
                    match parseDefinitionLine (index + 1) line with
                    | Ok definition -> loop (index + 1) (definition :: definitions) expressionLines false

                    | Error error -> Error error
            else
                loop (index + 1) definitions (line :: expressionLines) true

    loop 0 [] [] false

let private buildDefinitionMap definitions =
    definitions
    |> List.fold
        (fun map definition ->
            if Map.containsKey definition.Name map then
                failwithf "Duplicate definition '%s'." definition.Name
            else
                Map.add definition.Name definition.Body map)
        Map.empty

/// <summary>
/// Expands all named definitions inside the final expression.
/// </summary>
let expandDefinitions program =
    let definitions = buildDefinitionMap program.Definitions

    let definitionNames = definitions |> Map.toList |> List.map fst |> Set.ofList

    let rec expandTerm visiting term =
        let namesToExpand = freeVars term |> Set.intersect definitionNames |> Set.toList

        namesToExpand
        |> List.fold
            (fun currentTerm name ->
                let replacement = expandDefinition visiting name
                substitute currentTerm name replacement)
            term

    and expandDefinition visiting name =
        if Set.contains name visiting then
            failwithf "Cyclic definition involving '%s'." name
        else
            let body = Map.find name definitions
            expandTerm (Set.add name visiting) body

    expandTerm Set.empty program.Expression

/// <summary>
/// Parses, expands named definitions, beta-reduces, alpha-normalizes, and prints the result.
/// </summary>
let interpretString maxSteps input =
    match parseProgram input with
    | Error error -> Error error

    | Ok program ->
        try
            let expanded = expandDefinitions program

            match normalize maxSteps expanded with
            | Some normalized -> normalized |> alphaNormalize |> toString |> Ok

            | None -> Error(sprintf "Reduction did not finish in %d steps." maxSteps)
        with ex ->
            Error ex.Message

/// <summary>
/// Reads a lambda program from a file and interprets it.
/// </summary>
let interpretFile maxSteps path =
    File.ReadAllText(path) |> interpretString maxSteps
