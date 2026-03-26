// Copyright (c) Murat Khamatyanov. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace LocalNet

/// <summary>
/// Represents a computer in the local network with its operating system and infection state.
/// </summary>
type Computer(id: int, os: OperatingSystem, initiallyInfected: bool) =
    let mutable infected = initiallyInfected

    /// <summary>
    /// Gets the identifier of the computer.
    /// </summary>
    member _.Id = id

    /// <summary>
    /// Gets the operating system installed on the computer.
    /// </summary>
    member _.OS = os

    /// <summary>
    /// Gets a value indicating whether the computer is currently infected.
    /// </summary>
    member _.IsInfected = infected

    /// <summary>
    /// Marks the computer as infected.
    /// </summary>
    member _.Infect() =
        infected <- true

    /// <summary>
    /// Returns a readable string representation of the computer state.
    /// </summary>
    override _.ToString() =
        let osName =
            match os with
            | Windows -> "Windows"
            | Linux -> "Linux"
            | MacOS -> "MacOS"

        let state = if infected then "infected" else "healthy"
        $"Computer {id}: {osName}, {state}"