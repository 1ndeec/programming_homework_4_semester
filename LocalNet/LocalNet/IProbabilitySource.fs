// Copyright (c) Murat Khamatyanov. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace LocalNet

/// <summary>
/// Defines a source that decides whether an event with a given probability occurs.
/// </summary>
type IProbabilitySource =
    /// <summary>
    /// Determines whether an event with the specified probability happens.
    /// </summary>
    abstract member Happened : float -> bool