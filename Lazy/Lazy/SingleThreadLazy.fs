// Copyright (c) Murat Khamatyanov. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Lazy

/// <summary>
/// Represents a non-thread-safe implementation of a lazy value.
/// The value is computed once and then reused on later calls.
/// </summary>
type SingleThreadLazy<'a>(supplier : unit -> 'a) =
    let mutable isValueCreated = false
    let mutable value = Unchecked.defaultof<'a>

    interface ILazy<'a> with
        /// <summary>
        /// Returns the stored value if it has already been created.
        /// Otherwise computes it on the current thread without synchronization.
        /// </summary>
        member _.Get() =
            if not isValueCreated then
                value <- supplier()
                isValueCreated <- true
            value