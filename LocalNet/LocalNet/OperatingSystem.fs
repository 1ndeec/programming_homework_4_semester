// Copyright (c) Murat Khamatyanov. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace LocalNet

/// <summary>
/// Describes an operating system used in the infection model.
/// </summary>
type IOperatingSystem =
    /// <summary>
    /// Gets the operating system name.
    /// </summary>
    abstract member Name: string

    /// <summary>
    /// Gets the probability that this operating system is infected when exposed.
    /// </summary>
    abstract member InfectionProbability: float

/// <summary>
/// Represents an operating system used by a computer in the local network.
/// </summary>
type OperatingSystem(name: string, infectionProbability: float) =

    interface IOperatingSystem with
        member _.Name = name
        member _.InfectionProbability = infectionProbability

    /// <summary>
    /// Gets the operating system name.
    /// </summary>
    member _.Name = name

    /// <summary>
    /// Gets the probability that this operating system is infected when exposed.
    /// </summary>
    member _.InfectionProbability = infectionProbability

    /// <summary>
    /// Returns the operating system name.
    /// </summary>
    override _.ToString() = name
