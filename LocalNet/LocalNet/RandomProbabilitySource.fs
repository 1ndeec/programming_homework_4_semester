// Copyright (c) Murat Khamatyanov. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace LocalNet

/// <summary>
/// Provides random probability checks using a pseudorandom number generator.
/// </summary>
type RandomProbabilitySource() =
    let rnd = System.Random()

    interface IProbabilitySource with
        /// <summary>
        /// Determines randomly whether an event with the specified probability happens.
        /// </summary>
        member _.Happened(probability) =
            rnd.NextDouble() < probability