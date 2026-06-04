// Copyright (c) Murat Khamatyanov. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Lazy

open System.Threading

/// <summary>
/// Represents a thread-safe implementation of a lazy value.
/// The value is computed once and then returned on every later call.
/// </summary>
type ThreadSafeLazy<'a>(supplier : unit -> 'a) =
    let mutable isValueCreated = false
    let mutable value = Unchecked.defaultof<'a>
    let syncRoot = obj()

    interface ILazy<'a> with
        /// <summary>
        /// Returns the stored value if it has already been created.
        /// Otherwise computes it inside a lock to ensure thread safety.
        /// </summary>
        member _.Get() =
            if not (Volatile.Read(&isValueCreated)) then
                lock syncRoot (fun () ->
                    if not (Volatile.Read(&isValueCreated)) then
                        value <- supplier()
                        Volatile.Write(&isValueCreated, true)
                )

            value