// Copyright (c) Murat Khamatyanov. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

module GenericHomework.PhoneBookLogic

open System
open System.IO

type Record =
    { Name: string
      Phone: string }

type PhoneBook = Record list

let empty : PhoneBook = []

let addRecord (name: string) (phone: string) (book: PhoneBook) : PhoneBook =
    { Name = name; Phone = phone } :: book

let findPhonesByName (name: string) (book: PhoneBook) : string list =
    book
    |> List.filter (fun r -> r.Name = name)
    |> List.map (fun r -> r.Phone)

let findNamesByPhone (phone: string) (book: PhoneBook) : string list =
    book
    |> List.filter (fun r -> r.Phone = phone)
    |> List.map (fun r -> r.Name)

let getAllRecords (book: PhoneBook) : Record list =
    List.rev book

let toLines (book: PhoneBook) : string list =
    book
    |> getAllRecords
    |> List.map (fun r -> $"{r.Name};{r.Phone}")

let fromLines (lines: string list) : PhoneBook =
    lines
    |> List.choose (fun line ->
        match line.Split(';', 2, StringSplitOptions.None) with
        | [| name; phone |] -> Some { Name = name; Phone = phone }
        | _ -> None)

let saveToFile (path: string) (book: PhoneBook) : unit =
    File.WriteAllLines(path, toLines book)

let loadFromFile (path: string) : PhoneBook =
    if File.Exists path then
        File.ReadAllLines(path)
        |> Array.toList
        |> fromLines
    else
        empty