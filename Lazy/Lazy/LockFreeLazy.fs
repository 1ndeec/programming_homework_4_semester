// Copyright (c) Murat Khamatyanov. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Lazy

open System.Threading
/// <summary>
/// Boxed is created so we would be able to differ the actual null supplier from null, which the LockFreeLazy returns when task is not yet completed
/// </summary>
[<AllowNullLiteral>]
type private Boxed<'a>(value : 'a) =
    member _.Value = value

/// <summary>
/// Represents a lock-free implementation of a lazy value.
/// The value is published atomically and then reused on later calls.
/// </summary>
type LockFreeLazy<'a>(supplier : unit -> 'a) =
    let mutable boxed : Boxed<'a> = null

    interface ILazy<'a> with
        /// <summary>
        /// Returns the stored value if it has already been published.
        /// Otherwise computes the value and tries to publish it atomically.
        /// </summary>
        member _.Get() =
            let current = Volatile.Read(&boxed)
            if not (isNull current) then
                current.Value
            else
                let computed = Boxed(supplier())
                let original = Interlocked.CompareExchange(&boxed, computed, null)
                if isNull original then
                    computed.Value
                else
                    original.Value