// Copyright (c) Murat Khamatyanov. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace LocalNet

/// <summary>
/// Stores infection probabilities for each supported operating system.
/// </summary>
type InfectionConfig(windowsProbability: float, linuxProbability: float, macOsProbability: float) =

    /// <summary>
    /// Returns the infection probability associated with the specified operating system.
    /// </summary>
    member _.ForOs(os: OperatingSystem) =
        match os with
        | Windows -> windowsProbability
        | Linux -> linuxProbability
        | MacOS -> macOsProbability