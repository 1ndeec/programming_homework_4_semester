module Testwork.BlockingQueue

open System.Collections.Generic
open System.Threading

/// <summary>
/// Represents a thread-safe blocking queue that allows enqueueing items and waiting to dequeue them when the queue is empty.
/// </summary>
type BlockingQueue<'T>() =
    let queue = Queue<'T>()
    let syncRoot = obj()

    /// <summary>
    /// Adds an item to the queue and wakes one waiting thread.
    /// </summary>
    member _.Enqueue(item: 'T) =
        lock syncRoot (fun () ->
            queue.Enqueue(item)
            Monitor.Pulse(syncRoot)
        )

    /// <summary>
    /// Removes and returns an item from the queue, waiting if the queue is empty.
    /// </summary>
    member _.Dequeue() : 'T =
        lock syncRoot (fun () ->
            while queue.Count = 0 do
                Monitor.Wait(syncRoot) |> ignore

            queue.Dequeue()
        )

    /// <summary>
    /// Gets the number of items currently stored in the queue.
    /// </summary>
    member _.Count
        with get() =
            lock syncRoot (fun () -> queue.Count)

    /// <summary>
    /// Gets a value indicating whether the queue is empty.
    /// </summary>
    member _.IsEmpty
        with get() =
            lock syncRoot (fun () -> queue.Count = 0)