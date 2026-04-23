// Copyright (c) Murat Khamatyanov. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

module TestsForTestWork.BlockingQueueTests

open NUnit.Framework
open FsUnit
open System.Threading.Tasks
open Testwork.BlockingQueue

[<Test>]
let ``new queue is empty`` () =
    let q = BlockingQueue<int>()
    q.IsEmpty |> should equal true
    q.Count |> should equal 0

[<Test>]
let ``enqueue makes queue non empty`` () =
    let q = BlockingQueue<int>()
    q.Enqueue 42

    q.IsEmpty |> should equal false
    q.Count |> should equal 1

[<Test>]
let ``dequeue returns enqueued element`` () =
    let q = BlockingQueue<int>()
    q.Enqueue 42

    q.Dequeue() |> should equal 42

[<Test>]
let ``queue works in fifo order`` () =
    let q = BlockingQueue<int>()
    q.Enqueue 10
    q.Enqueue 20
    q.Enqueue 30

    q.Dequeue() |> should equal 10
    q.Dequeue() |> should equal 20
    q.Dequeue() |> should equal 30

[<Test>]
let ``count changes after enqueue and dequeue`` () =
    let q = BlockingQueue<int>()
    q.Count |> should equal 0

    q.Enqueue 1
    q.Enqueue 2
    q.Count |> should equal 2

    q.Dequeue() |> ignore
    q.Count |> should equal 1

    q.Dequeue() |> ignore
    q.Count |> should equal 0
    q.IsEmpty |> should equal true

[<Test>]
let ``dequeue blocks until item appears`` () =
    let q = BlockingQueue<int>()

    let task = Task.Run(fun () -> q.Dequeue())

    Task.Delay(100).Wait()
    task.IsCompleted |> should equal false

    q.Enqueue 99

    task.Wait(1000) |> should equal true
    task.Result |> should equal 99

[<Test>]
let ``consumer can dequeue several produced items in order`` () =
    let q = BlockingQueue<int>()

    let consumer =
        Task.Run(fun () ->
            [ q.Dequeue()
              q.Dequeue()
              q.Dequeue() ])

    Task.Delay(100).Wait()
    consumer.IsCompleted |> should equal false

    q.Enqueue 1
    q.Enqueue 2
    q.Enqueue 3

    consumer.Wait(1000) |> should equal true
    consumer.Result |> should equal [1; 2; 3]