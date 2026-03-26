// Copyright (c) Murat Khamatyanov. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace LocalNet

/// <summary>
/// Represents a local network of computers and controls the step-by-step virus spread simulation.
/// </summary>
type LocalNetwork
    (
        computers: Computer[],
        adjacencyMatrix: bool[,],
        infectionConfig: InfectionConfig,
        probabilitySource: IProbabilitySource
    ) =

    /// <summary>
    /// Gets the collection of computers in the network.
    /// </summary>
    member _.Computers = computers

    /// <summary>
    /// Prints the current state of the network for the specified simulation step.
    /// </summary>
    member _.PrintState(step: int) =
        printfn $"Step {step}:"
        computers |> Array.iter (fun c -> printfn $"  {c}")
        printfn ""

    /// <summary>
    /// Performs one discrete simulation step of virus spreading.
    /// </summary>
    member _.Step() =
        let infectedAtStart =
            computers
            |> Array.mapi (fun i c -> i, c)
            |> Array.filter (fun (_, c) -> c.IsInfected)
            |> Array.map fst
            |> Set.ofArray

        let toTryInfect =
            computers
            |> Array.mapi (fun i c -> i, c)
            |> Array.choose (fun (targetIndex, targetComputer) ->
                if targetComputer.IsInfected then
                    None
                else
                    let hasInfectedNeighbor =
                        infectedAtStart
                        |> Set.exists (fun infectedIndex -> adjacencyMatrix[infectedIndex, targetIndex])

                    if hasInfectedNeighbor then Some targetComputer else None)

        toTryInfect
        |> Array.iter (fun computer ->
            let p = infectionConfig.ForOs(computer.OS)
            if probabilitySource.Happened(p) then
                computer.Infect())

    /// <summary>
    /// Checks whether the network state can still change in future steps.
    /// </summary>
    member this.CanStillChange() =
        computers
        |> Array.mapi (fun i c -> i, c)
        |> Array.exists (fun (i, c) ->
            not c.IsInfected &&
            (computers
             |> Array.mapi (fun j other -> j, other)
             |> Array.exists (fun (j, other) ->
                 other.IsInfected && adjacencyMatrix[j, i])))

    /// <summary>
    /// Runs the simulation and prints the network state after each step until no further changes are possible.
    /// </summary>
    member this.Run() =
        let mutable step = 0
        this.PrintState(step)

        while this.CanStillChange() do
            step <- step + 1
            this.Step()
            this.PrintState(step)