// Copyright (c) Murat Khamatyanov. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

module LambdaInterpreter.Reducer

open LambdaInterpreter.Ast

/// Finds all free variables in a lambda term.
let rec freeVars term =
    match term with
    | Var name -> Set.singleton name
    | App(left, right) -> Set.union (freeVars left) (freeVars right)
    | Lam(param, body) -> Set.remove param (freeVars body)

/// Finds all variable names used anywhere in a lambda term.
let rec private allVars term =
    match term with
    | Var name -> Set.singleton name
    | App(left, right) -> Set.union (allVars left) (allVars right)
    | Lam(param, body) -> Set.add param (allVars body)

/// Generates a variable name that is not present in the forbidden set.
let private freshName baseName forbidden =
    let baseName =
        if System.String.IsNullOrWhiteSpace baseName then
            "v"
        else
            baseName

    let rec loop index =
        let candidate = $"{baseName}_{index}"

        if Set.contains candidate forbidden then
            loop (index + 1)
        else
            candidate

    if not (Set.contains baseName forbidden) then
        baseName
    else
        loop 1

/// Renames free occurrences of a variable inside the current binding scope.
let rec private renameBound oldName newName term =
    match term with
    | Var name when name = oldName -> Var newName
    | Var _ -> term

    | App(left, right) -> App(renameBound oldName newName left, renameBound oldName newName right)

    | Lam(param, body) when param = oldName -> Lam(param, body)

    | Lam(param, body) -> Lam(param, renameBound oldName newName body)

/// Performs capture-avoiding substitution of a replacement term for a variable.
let substitute variable replacement term =
    // Free variables of the replacement term that must not be accidentally captured.
    let replacementFreeVars = freeVars replacement

    // Recursive substitution function over the structure of the term.
    let rec subst term =
        match term with
        | Var name when name = variable -> replacement
        | Var _ -> term

        | App(left, right) -> App(subst left, subst right)

        | Lam(param, body) when param = variable -> Lam(param, body)

        | Lam(param, body) ->
            let variableActuallyOccurs = Set.contains variable (freeVars body)

            let captureWouldHappen = Set.contains param replacementFreeVars

            if not variableActuallyOccurs || not captureWouldHappen then
                Lam(param, subst body)
            else
                // Names that cannot be used for alpha-renaming.
                let forbidden =
                    Set.union (Set.union (allVars body) (allVars replacement)) (Set.ofList [ variable; param ])

                let fresh = freshName param forbidden

                let renamedBody = renameBound param fresh body

                Lam(fresh, subst renamedBody)

    subst term

/// Replaces free occurrences of named definitions in the final program expression.
let expandDefinitions definitions expression =
    let definitions =
        definitions
        |> List.fold (fun acc definition -> Map.add definition.Name definition.Body acc) Map.empty

    let rec expandTerm boundNames visitingDefinitions term =
        match term with
        | Var name when Set.contains name boundNames -> Var name

        | Var name ->
            match Map.tryFind name definitions with
            | None -> Var name
            | Some body ->
                if Set.contains name visitingDefinitions then
                    failwith $"Cyclic definition involving '{name}'."
                else
                    expandTerm Set.empty (Set.add name visitingDefinitions) body

        | App(left, right) ->
            App(expandTerm boundNames visitingDefinitions left, expandTerm boundNames visitingDefinitions right)

        | Lam(param, body) -> Lam(param, expandTerm (Set.add param boundNames) visitingDefinitions body)

    let expandedDefinitions =
        definitions
        |> Map.map (fun name body -> expandTerm Set.empty (Set.singleton name) body)

    expandedDefinitions
    |> Map.toList
    |> List.fold (fun acc (name, body) -> substitute name body acc) expression

/// Performs one beta-reduction step using normal-order strategy.
let rec reduceOnceNormal term =
    match term with
    | App(Lam(param, body), argument) -> Some(substitute param argument body)

    | App(left, right) ->
        match reduceOnceNormal left with
        | Some reducedLeft -> Some(App(reducedLeft, right))
        | None ->
            match reduceOnceNormal right with
            | Some reducedRight -> Some(App(left, reducedRight))
            | None -> None

    | Lam(param, body) -> reduceOnceNormal body |> Option.map (fun reducedBody -> Lam(param, reducedBody))

    | Var _ -> None

/// Fully reduces a term until normal form or until the step limit is reached.
let normalize maxSteps term =
    let rec loop steps current =
        if steps >= maxSteps then
            Result.Error $"Reduction did not terminate in {maxSteps} steps."
        else
            match reduceOnceNormal current with
            | None -> Result.Ok current
            | Some next -> loop (steps + 1) next

    loop 0 term

/// Renames bound variables into a stable canonical form.
let alphaNormalize term =
    let preferredNames = "xyzuvwabcdefghijkmnpqrst" |> Seq.map string |> Seq.toArray

    let globallyForbidden = freeVars term

    // Returns the preferred name for a given index.
    let nameAt index =
        if index < preferredNames.Length then
            preferredNames[index]
        else
            $"x{index - preferredNames.Length + 1}"

    let rec choose index used =
        let candidate = nameAt index

        if Set.contains candidate used || Set.contains candidate globallyForbidden then
            choose (index + 1) used
        else
            candidate, index + 1

    // Recursively normalizes bound variable names.
    let rec normalize env used nextName term =
        match term with
        | Var name ->
            let normalizedName = Map.tryFind name env |> Option.defaultValue name

            Var normalizedName, nextName

        | App(left, right) ->
            let normalizedLeft, nextAfterLeft = normalize env used nextName left

            let normalizedRight, nextAfterRight = normalize env used nextAfterLeft right

            App(normalizedLeft, normalizedRight), nextAfterRight

        | Lam(param, body) ->
            let newName, nextAfterChoice = choose nextName used

            let normalizedBody, nextAfterBody =
                normalize (Map.add param newName env) (Set.add newName used) nextAfterChoice body

            Lam(newName, normalizedBody), nextAfterBody

    normalize Map.empty Set.empty 0 term |> fst
