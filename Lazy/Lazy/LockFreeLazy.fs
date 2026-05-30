// Copyright (c) Murat Khamatyanov. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Lazy

open System
open System.Threading

/// <summary>
/// Boxed is created so we would be able to differ the actual null supplier from null, which the LockFreeLazy returns when task is not yet completed
/// </summary>
[<AllowNullLiteral>]
type private Boxed<'a>(value : 'a) =
    member _.Value = value

/// <summary>
/// Represents a CAS-based lazy value.
/// Exactly one thread is allowed to compute the value.
/// Other threads wait until the computed value is published.
/// </summary>
type LockFreeLazy<'a>(supplier: unit -> 'a) =
    let calculating = obj()
    let mutable state: obj = null

    interface ILazy<'a> with
        /// <summary>
        /// Returns the stored value if it has already been computed.
        /// Otherwise one thread becomes the computing thread, calls supplier,
        /// and publishes the result. Other threads spin until the result appears.
        /// </summary>
        member _.Get() =
            let spinner = SpinWait()

            let rec loop () =
                let current = Volatile.Read(&state)

                if isNull current then
                    let previous =
                        Interlocked.CompareExchange(&state, calculating, null)

                    if isNull previous then
                        try
                            let computed = Boxed(supplier())
                            Volatile.Write(&state, computed :> obj)
                            computed.Value
                        with
                        | _ ->
                            Volatile.Write(&state, null)
                            reraise()
                    else
                        loop ()

                elif Object.ReferenceEquals(current, calculating) then
                    spinner.SpinOnce()
                    loop ()

                else
                    (current :?> Boxed<'a>).Value

            loop ()