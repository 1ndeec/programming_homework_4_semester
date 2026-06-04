// Copyright (c) Murat Khamatyanov. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Lazy

open System.Threading

/// <summary>
/// Represents a CAS-based lazy value.
/// Several threads may compute the value, but only one computed value is published.
/// </summary>
type LockFreeLazy<'a>(supplier: unit -> 'a) =
    let mutable state: 'a option = None

    interface ILazy<'a> with
        /// <summary>
        /// Returns the stored value if it has already been computed.
        /// Otherwise computes a value and publishes it if another thread has not done so yet.
        /// </summary>
        member _.Get() =
            match Volatile.Read(&state) with
            | Some value -> value
            | None ->
                let computed = supplier()
                let previous = Interlocked.CompareExchange(&state, Some computed, None)

                match previous with
                | Some value -> value
                | None -> computed
