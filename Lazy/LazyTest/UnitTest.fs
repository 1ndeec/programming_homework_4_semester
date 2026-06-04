// Copyright (c) Murat Khamatyanov. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace LazyTests

open System
open System.Threading
open System.Threading.Tasks
open NUnit.Framework

open FsUnit
open Lazy

[<TestFixture>]
type ``SingleThreadLazy tests`` () =

    [<Test>]
    member _.``Get computes value on first call and returns it`` () =
        let lazyValue =
            SingleThreadLazy(fun () -> 42) :> ILazy<int>

        lazyValue.Get() |> should equal 42

    [<Test>]
    member _.``Get caches result and supplier is called once`` () =
        let mutable calls = 0

        let lazyValue =
            SingleThreadLazy(fun () ->
                calls <- calls + 1
                42) :> ILazy<int>

        lazyValue.Get() |> ignore
        lazyValue.Get() |> ignore
        lazyValue.Get() |> ignore

        calls |> should equal 1

    [<Test>]
    member _.``Repeated Get returns the same reference`` () =
        let lazyValue =
            SingleThreadLazy(fun () -> obj()) :> ILazy<obj>

        let first = lazyValue.Get()
        let second = lazyValue.Get()

        second |> should be (sameAs first)


[<TestFixture>]
type ``ThreadSafeLazy tests`` () =

    [<Test>]
    member _.``Get computes value on first call and returns it`` () =
        let lazyValue =
            ThreadSafeLazy(fun () -> 42) :> ILazy<int>

        lazyValue.Get() |> should equal 42

    [<Test>]
    member _.``Sequential calls compute only once`` () =
        let mutable calls = 0

        let lazyValue =
            ThreadSafeLazy(fun () ->
                calls <- calls + 1
                42) :> ILazy<int>

        lazyValue.Get() |> ignore
        lazyValue.Get() |> ignore
        lazyValue.Get() |> ignore

        calls |> should equal 1

    [<Test>]
    member _.``Concurrent calls still compute only once`` () =
        let calls = ref 0

        let lazyValue =
            ThreadSafeLazy(fun () ->
                Interlocked.Increment(calls) |> ignore
                Thread.Sleep(50)
                obj()) :> ILazy<obj>

        let results =
            [| 1 .. 32 |]
            |> Array.map (fun _ -> Task.Run(fun () -> lazyValue.Get()))
            |> Task.WhenAll
            |> fun t -> t.Result

        (!calls) |> should equal 1

        let first = results[0]
        results |> Array.iter (fun x -> x |> should be (sameAs first))


[<TestFixture>]
type ``LockFreeLazy tests`` () =

    [<Test>]
    member _.``Get computes value on first call and returns it`` () =
        let lazyValue =
            LockFreeLazy(fun () -> 42) :> ILazy<int>

        lazyValue.Get() |> should equal 42

    [<Test>]
    member _.``Sequential calls return the same reference`` () =
        let lazyValue =
            LockFreeLazy(fun () -> obj()) :> ILazy<obj>

        let first = lazyValue.Get()
        let second = lazyValue.Get()
        let third = lazyValue.Get()

        second |> should be (sameAs first)
        third |> should be (sameAs first)

    [<Test>]
    member _.``Concurrent calls may compute more than once but must return one published object`` () =
        let calls = ref 0

        let lazyValue =
            LockFreeLazy(fun () ->
                Interlocked.Increment(calls) |> ignore
                Thread.Sleep(50)
                obj()) :> ILazy<obj>

        let results =
            [| 1 .. 32 |]
            |> Array.map (fun _ -> Task.Run(fun () -> lazyValue.Get()))
            |> Task.WhenAll
            |> fun t -> t.Result

        (!calls) |> should be (greaterThanOrEqualTo 1)

        let first = results[0]
        results |> Array.iter (fun x -> x |> should be (sameAs first))

    [<Test>]
    member _.``LockFreeLazy handles null result correctly`` () =
        let mutable calls = 0

        let lazyValue =
            LockFreeLazy<string>(fun () ->
                calls <- calls + 1
                null) :> ILazy<string>

        let first = lazyValue.Get()
        let second = lazyValue.Get()
        let third = lazyValue.Get()

        first |> should equal null
        second |> should equal null
        third |> should equal null
        calls |> should equal 1
