// Copyright (c) Murat Khamatyanov. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

module GenericHomework.PhoneBookFace
open System
open GenericHomework.PhoneBookLogic

let printMenu () =
    printfn ""
    printfn "1 - Add record"
    printfn "2 - Find phone by name"
    printfn "3 - Find name by phone"
    printfn "4 - Show all records"
    printfn "5 - Save to file"
    printfn "6 - Load from file"
    printfn "0 - Exit"
    printf "Choose: "

let printStringList header items =
    match items with
    | [] -> printfn "%s: nothing found" header
    | _ ->
        printfn "%s:" header
        items |> List.iter (printfn "%s")

let printRecords (book: PhoneBook) =
    match getAllRecords book with
    | [] -> printfn "Phone book is empty"
    | records ->
        printfn "Current records:"
        records |> List.iter (fun r -> printfn "Name: %s, Phone: %s" r.Name r.Phone)

let rec loop (book: PhoneBook) =
    printMenu ()
    match Console.ReadLine() with
    | "1" ->
        printf "Enter name: "
        let name = Console.ReadLine()
        printf "Enter phone: "
        let phone = Console.ReadLine()
        let newBook = addRecord name phone book
        printfn "Record added"
        loop newBook

    | "2" ->
        printf "Enter name: "
        let name = Console.ReadLine()
        findPhonesByName name book |> printStringList "Phones"
        loop book

    | "3" ->
        printf "Enter phone: "
        let phone = Console.ReadLine()
        findNamesByPhone phone book |> printStringList "Names"
        loop book

    | "4" ->
        printRecords book
        loop book

    | "5" ->
        printf "Enter file path: "
        let path = Console.ReadLine()
        saveToFile path book
        printfn "Saved"
        loop book

    | "6" ->
        printf "Enter file path: "
        let path = Console.ReadLine()
        let newBook = loadFromFile path
        printfn "Loaded"
        loop newBook

    | "0" ->
        printfn "Bye"

    | _ ->
        printfn "Unknown command"
        loop book