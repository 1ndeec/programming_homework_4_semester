// Copyright (c) Murat Khamatyanov. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

module GenericHomework.BraceSequence


let isOpening ch =
    ch = '(' || ch = '[' || ch = '{'

let matches openBracket closeBracket =
    (openBracket = '(' && closeBracket = ')')
    || (openBracket = '[' && closeBracket = ']')
    || (openBracket = '{' && closeBracket = '}')

let isCorrectBracketSequence (text: string) =
    let rec loop index stack =
        if index = text.Length then
            List.isEmpty stack
        else
            let ch = text[index]

            if isOpening ch then
                loop (index + 1) (ch :: stack)
            elif ch = ')' || ch = ']' || ch = '}' then
                match stack with
                | top :: rest when matches top ch ->
                    loop (index + 1) rest
                | _ ->
                    false
            else
                loop (index + 1) stack

    loop 0 []