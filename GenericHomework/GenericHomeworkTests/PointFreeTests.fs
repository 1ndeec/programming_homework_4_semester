module GenericHomeworkTests.PointFreeTests

open NUnit.Framework
open FsCheck
open GenericHomework.PointFree

[<Test>]
let ``point-free version should be equivalent to original on random data`` () =
    Check.QuickThrowOnFailure
        (fun (x: int) (l: int list) ->
            func0 x l = func x l)

[<Test>]
let ``all intermediate versions should be equivalent`` () =
    Check.QuickThrowOnFailure
        (fun (x: int) (l: int list) ->
            let expected = func0 x l
            expected = func1 x l
            && expected = func2 x l
            && expected = func3 x l
            && expected = func4 x l
            && expected = func x l)

[<Test>]
let ``result length should be preserved`` () =
    Check.QuickThrowOnFailure
        (fun (x: int) (l: int list) ->
            List.length (func x l) = List.length l)

[<Test>]
let ``point-free version should multiply every element by x`` () =
    Check.QuickThrowOnFailure
        (fun (x: int) (l: int list) ->
            func x l = List.map (fun y -> x * y) l)