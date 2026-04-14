// Copyright (c) Murat Khamatyanov. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Lazy

/// <summary>
/// Returns the cached value or computes it on the first call.
/// </summary>
type ILazy<'a> =
    abstract member Get : unit -> 'a