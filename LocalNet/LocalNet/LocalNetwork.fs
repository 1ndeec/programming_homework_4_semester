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

        let toTryInfect =
            infectedAtStart
            |> Seq.collect (fun (infectedIndex, _) ->
                computers
                |> Array.mapi (fun targetIndex targetComputer -> targetIndex, targetComputer)
                |> Array.choose (fun (targetIndex, targetComputer) ->
                    if
                        adjacencyMatrix[infectedIndex, targetIndex]
                        && not targetComputer.IsInfected
                        && targetComputer.OS.InfectionProbability > 0.0
                    then
                        Some targetIndex
                    else
                        None))
            |> Set.ofSeq

        toTryInfect
        |> Set.iter (fun targetIndex ->
            let computer = computers[targetIndex]
            let p = computer.OS.InfectionProbability

            if p > 0.0 && probabilitySource.Happened(p) then
                computer.Infect())

    /// <summary>
    /// Checks whether the network state can still change in future steps.
    /// </summary>
    member this.CanStillChange() =
        computers
        |> Array.mapi (fun i c -> i, c)
        |> Array.exists (fun (i, c) ->
            not c.IsInfected &&
            c.OS.InfectionProbability > 0.0 &&
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
