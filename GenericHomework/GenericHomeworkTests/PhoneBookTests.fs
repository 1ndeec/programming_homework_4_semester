// Copyright (c) Murat Khamatyanov. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

module GenericHomeworkTests.PhoneBookTests

open NUnit.Framework
open FsUnit
open GenericHomework.PhoneBookLogic

[<Test>]
let ``empty phone book should contain no records`` () =
    getAllRecords empty |> should equal ([] : Record list)

[<Test>]
let ``added record should be searchable by name`` () =
    let book =
        empty
        |> addRecord "Alice" "111"

    findPhonesByName "Alice" book |> should equal [ "111" ]

[<Test>]
let ``added record should be searchable by phone`` () =
    let book =
        empty
        |> addRecord "Alice" "111"

    findNamesByPhone "111" book |> should equal [ "Alice" ]

[<Test>]
let ``multiple phones for one name should be returned`` () =
    let book =
        empty
        |> addRecord "Alice" "111"
        |> addRecord "Alice" "222"

    findPhonesByName "Alice" book |> List.sort |> should equal [ "111"; "222" ]

[<Test>]
let ``multiple names for one phone should be returned`` () =
    let book =
        empty
        |> addRecord "Alice" "111"
        |> addRecord "Bob" "111"

    findNamesByPhone "111" book |> List.sort |> should equal [ "Alice"; "Bob" ]

[<Test>]
let ``toLines and fromLines should preserve data`` () =
    let book =
        empty
        |> addRecord "Alice" "111"
        |> addRecord "Bob" "222"

    let restored =
        book
        |> toLines
        |> fromLines

    let sortRecords records =
        records |> List.sortBy (fun r -> r.Name, r.Phone)

    getAllRecords restored |> sortRecords
    |> should equal (getAllRecords book |> sortRecords)