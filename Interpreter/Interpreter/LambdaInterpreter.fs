// Copyright (c) Murat Khamatyanov. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

module Testwork.LambdaInterpreter

open System

/// <summary>
/// Represents an untyped lambda calculus term:
/// variable, application, or lambda abstraction.
/// </summary>
type Term =
    | Var of string
    | App of Term * Term
    | Lam of string * Term

/// <summary>
/// Returns all free variables of the given lambda term.
/// </summary>
let rec freeVars term =
    match term with
    | Var name -> Set.singleton name
    | App(left, right) -> Set.union (freeVars left) (freeVars right)
    | Lam(parameter, body) -> Set.remove parameter (freeVars body)

/// <summary>
/// Returns all variables appearing in the given lambda term.
/// </summary>
let rec private allVars term =
    match term with
    | Var name -> Set.singleton name
    | App(left, right) -> Set.union (allVars left) (allVars right)
    | Lam(parameter, body) -> Set.add parameter (allVars body)

let private freshName baseName used =
    let baseName =
        if String.IsNullOrWhiteSpace baseName then
            "v"
        else
            baseName

    if not (Set.contains baseName used) then
        baseName
    else
        Seq.initInfinite (fun i -> sprintf "%s%d" baseName i)
        |> Seq.find (fun candidate -> not (Set.contains candidate used))

/// <summary>
/// Performs capture-avoiding substitution:
/// replaces all free occurrences of the given variable with the replacement term.
/// </summary>
let rec substitute term variable replacement =
    match term with
    | Var name when name = variable -> replacement
    | Var _ -> term

    | App(left, right) ->
        App(
            substitute left variable replacement,
            substitute right variable replacement
        )

    | Lam(parameter, body) when parameter = variable ->
        Lam(parameter, body)

    | Lam(parameter, body) ->
        let replacementFreeVars = freeVars replacement
        let bodyFreeVars = freeVars body

        if not (Set.contains parameter replacementFreeVars)
           || not (Set.contains variable bodyFreeVars) then
            Lam(parameter, substitute body variable replacement)
        else
            let usedVars =
                Set.union (allVars body) (allVars replacement)
                |> Set.add parameter
                |> Set.add variable

            let newParameter = freshName parameter usedVars
            let renamedBody = substitute body parameter (Var newParameter)

            Lam(newParameter, substitute renamedBody variable replacement)

/// <summary>
/// Performs one beta-reduction step using normal strategy:
/// the leftmost outermost reducible expression is reduced first.
/// </summary>
let rec reduceOnce term =
    match term with
    | App(Lam(parameter, body), argument) ->
        Some(substitute body parameter argument)

    | App(left, right) ->
        match reduceOnce left with
        | Some reducedLeft -> Some(App(reducedLeft, right))
        | None ->
            match reduceOnce right with
            | Some reducedRight -> Some(App(left, reducedRight))
            | None -> None

    | Lam(parameter, body) ->
        match reduceOnce body with
        | Some reducedBody -> Some(Lam(parameter, reducedBody))
        | None -> None

    | Var _ -> None

/// <summary>
/// Reduces the given term to normal form using at most maxSteps beta-reduction steps.
/// Returns None if the step limit is exceeded.
/// </summary>
let normalize maxSteps term =
    if maxSteps < 0 then
        invalidArg "maxSteps" "Step limit must be non-negative."

    let rec loop remainingSteps current =
        match reduceOnce current with
        | None -> Some current
        | Some _ when remainingSteps = 0 -> None
        | Some next -> loop (remainingSteps - 1) next

    loop maxSteps term

/// <summary>
/// Interprets the given lambda term by reducing it to normal form.
/// </summary>
let interpret maxSteps term =
    normalize maxSteps term

/// <summary>
/// Renames bound variables to canonical names.
/// Useful for comparing lambda terms up to alpha-equivalence.
/// </summary>
let alphaNormalize term =
    let initialUsedVars = freeVars term

    let rec nextName index usedVars =
        let candidate = sprintf "__v%d" index

        if Set.contains candidate usedVars then
            nextName (index + 1) usedVars
        else
            candidate

    let rec normalizeInner environment usedVars counter term =
        match term with
        | Var name ->
            let normalizedName =
                match Map.tryFind name environment with
                | Some newName -> newName
                | None -> name

            Var normalizedName, usedVars, counter

        | App(left, right) ->
            let normalizedLeft, usedVarsAfterLeft, counterAfterLeft =
                normalizeInner environment usedVars counter left

            let normalizedRight, usedVarsAfterRight, counterAfterRight =
                normalizeInner environment usedVarsAfterLeft counterAfterLeft right

            App(normalizedLeft, normalizedRight), usedVarsAfterRight, counterAfterRight

        | Lam(parameter, body) ->
            let newParameter = nextName counter usedVars
            let newUsedVars = Set.add newParameter usedVars

            let normalizedBody, finalUsedVars, finalCounter =
                normalizeInner
                    (Map.add parameter newParameter environment)
                    newUsedVars
                    (counter + 1)
                    body

            Lam(newParameter, normalizedBody), finalUsedVars, finalCounter

    let normalizedTerm, _, _ =
        normalizeInner Map.empty initialUsedVars 0 term

    normalizedTerm

/// <summary>
/// Checks whether two lambda terms are equal up to alpha-conversion.
/// </summary>
let alphaEquivalent first second =
    alphaNormalize first = alphaNormalize second

let rec private collectLambdaArgs term =
    match term with
    | Lam(parameter, body) ->
        let parameters, finalBody = collectLambdaArgs body
        parameter :: parameters, finalBody
    | _ -> [], term

let private parenthesize condition text =
    if condition then
        "(" + text + ")"
    else
        text

let rec private format precedence term =
    match term with
    | Var name -> name

    | Lam _ ->
        let parameters, body = collectLambdaArgs term

        let text =
            "\\" + String.concat " " parameters + "." + format 0 body

        parenthesize (precedence > 0) text

    | App(left, right) ->
        let text = format 1 left + " " + format 2 right
        parenthesize (precedence > 1) text

/// <summary>
/// Converts a lambda term to a readable string representation.
/// </summary>
let toString term =
    format 0 term