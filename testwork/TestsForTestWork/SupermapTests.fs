// Copyright (c) Murat Khamatyanov. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

module TestsForTestWork.Supermap

open NUnit.Framework
open FsUnit
open Testwork.Supermap

[<Test>]
let ``supermap returns empty list for empty input`` () =
    supermap [] (fun x -> [x])
    |> should equal []

[<Test>]
let ``supermap applies mapper to each element and concatenates results`` () =
    supermap [1; 2; 3] (fun x -> [x; x * 10])
    |> should equal [1; 10; 2; 20; 3; 30]

[<Test>]
let ``supermap works when mapper returns empty list`` () =
    supermap [1; 2; 3] (fun _ -> [])
    |> should equal []

[<Test>]
let ``supermap with singleton lists behaves like map`` () =
    supermap [1; 2; 3; 4] (fun x -> [x + 1])
    |> should equal [2; 3; 4; 5]

[<Test>]
let ``supermap preserves order of produced elements`` () =
    supermap [1; 2; 3] (fun x -> [x; -x])
    |> should equal [1; -1; 2; -2; 3; -3]

[<Test>]
let ``supermap can change element type`` () =
    supermap [1; 2; 3] (fun x -> [string x; string (x * x)])
    |> should equal ["1"; "1"; "2"; "4"; "3"; "9"]

[<Test>]
let ``supermap duplicates every element`` () =
    supermap [1; 2; 3] (fun x -> [x; x])
    |> should equal [1; 1; 2; 2; 3; 3]