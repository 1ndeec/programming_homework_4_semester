// Copyright (c) Murat Khamatyanov. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace LocalNet.Tests

open NUnit.Framework
open FsUnit
open LocalNet

/// <summary>
/// Probability source that always reports infection success.
/// </summary>
type AlwaysInfectProbabilitySource() =
    interface IProbabilitySource with
        member _.Happened(_) = true

/// <summary>
/// Probability source that always reports infection failure.
/// </summary>
type NeverInfectProbabilitySource() =
    interface IProbabilitySource with
        member _.Happened(_) = false

/// <summary>
/// Probability source that returns success only when the received probability matches the expected value.
/// </summary>
type ProbabilityEqualsSource(expected: float) =
    interface IProbabilitySource with
        member _.Happened(p) = (p = expected)

/// <summary>
/// Tests for infection propagation and probability selection in the local network model.
/// </summary>
[<TestFixture>]
type LocalNetworkTests() =

    /// <summary>
    /// Creates a line graph with seven computers where only the first computer is infected initially.
    /// </summary>
    let createSevenNodeLineNetwork windowsProbability linuxProbability macOsProbability probabilitySource =
        let windows = OperatingSystem("Windows", windowsProbability)
        let linux = OperatingSystem("Linux", linuxProbability)
        let macOs = OperatingSystem("MacOS", macOsProbability)

        let computers =
            [|
                Computer(0, windows, true)
                Computer(1, linux, false)
                Computer(2, macOs, false)
                Computer(3, windows, false)
                Computer(4, linux, false)
                Computer(5, macOs, false)
                Computer(6, windows, false)
            |]

        let adjacency =
            array2D [|
                [| false; true;  false; false; false; false; false |]
                [| true;  false; true;  false; false; false; false |]
                [| false; true;  false; true;  false; false; false |]
                [| false; false; true;  false; true;  false; false |]
                [| false; false; false; true;  false; true;  false |]
                [| false; false; false; false; true;  false; true  |]
                [| false; false; false; false; false; true;  false |]
            |]

        LocalNetwork(computers, adjacency, probabilitySource)

    /// <summary>
    /// Verifies that with unit infection probabilities the virus spreads one level per step through the 7-node line graph.
    /// </summary>
    [<Test>]
    member _.``All one probabilities spread level by level on 7-node graph``() =
        let network =
            createSevenNodeLineNetwork
                1.0
                1.0
                1.0
                (AlwaysInfectProbabilitySource())

        network.Step()
        network.Computers[0].IsInfected |> should equal true
        network.Computers[1].IsInfected |> should equal true
        network.Computers[2].IsInfected |> should equal false
        network.Computers[3].IsInfected |> should equal false
        network.Computers[4].IsInfected |> should equal false
        network.Computers[5].IsInfected |> should equal false
        network.Computers[6].IsInfected |> should equal false

        network.Step()
        network.Computers[2].IsInfected |> should equal true
        network.Computers[3].IsInfected |> should equal false

        network.Step()
        network.Computers[3].IsInfected |> should equal true
        network.Computers[4].IsInfected |> should equal false

        network.Step()
        network.Computers[4].IsInfected |> should equal true
        network.Computers[5].IsInfected |> should equal false

        network.Step()
        network.Computers[5].IsInfected |> should equal true
        network.Computers[6].IsInfected |> should equal false

        network.Step()
        network.Computers[6].IsInfected |> should equal true

    /// <summary>
    /// Verifies that infection cannot pass across two edges during a single simulation step.
    /// </summary>
    [<Test>]
    member _.``All one probabilities do not jump over two edges in one step``() =
        let network =
            createSevenNodeLineNetwork
                1.0
                1.0
                1.0
                (AlwaysInfectProbabilitySource())

        network.Step()

        network.Computers[2].IsInfected |> should equal false
        network.Computers[3].IsInfected |> should equal false
        network.Computers[4].IsInfected |> should equal false
        network.Computers[5].IsInfected |> should equal false
        network.Computers[6].IsInfected |> should equal false

    /// <summary>
    /// Verifies that with zero infection probabilities no new computers become infected.
    /// </summary>
    [<Test>]
    member _.``All zero probabilities infect nobody new on 7-node graph``() =
        let network =
            createSevenNodeLineNetwork
                0.0
                0.0
                0.0
                (NeverInfectProbabilitySource())

        network.Step()
        network.Step()
        network.Step()

        network.Computers[0].IsInfected |> should equal true
        network.Computers[1].IsInfected |> should equal false
        network.Computers[2].IsInfected |> should equal false
        network.Computers[3].IsInfected |> should equal false
        network.Computers[4].IsInfected |> should equal false
        network.Computers[5].IsInfected |> should equal false
        network.Computers[6].IsInfected |> should equal false

    /// <summary>
    /// Verifies that the network cannot change when exposed healthy computers have zero infection probability.
    /// </summary>
    [<Test>]
    member _.``All zero probabilities cannot change while exposed probability is zero``() =
        let network =
            createSevenNodeLineNetwork
                0.0
                0.0
                0.0
                (NeverInfectProbabilitySource())

        network.CanStillChange() |> should equal false

    /// <summary>
    /// Verifies that the Linux infection probability is used for the first exposed neighbour.
    /// </summary>
    [<Test>]
    member _.``Mixed probabilities use Linux probability 0.4 for node 1``() =
        let network =
            createSevenNodeLineNetwork
                0.7
                0.4
                0.1
                (ProbabilityEqualsSource(0.4))

        network.Step()

        network.Computers[1].IsInfected |> should equal true
        network.Computers[2].IsInfected |> should equal false
        network.Computers[3].IsInfected |> should equal false
        network.Computers[4].IsInfected |> should equal false
        network.Computers[5].IsInfected |> should equal false
        network.Computers[6].IsInfected |> should equal false

    /// <summary>
    /// Verifies that a computer is not infected when the probability source expects a value different from the configured one.
    /// </summary>
    [<Test>]
    member _.``Mixed probabilities do not infect node 1 if probability source expects wrong value``() =
        let network =
            createSevenNodeLineNetwork
                0.7
                0.4
                0.1
                (ProbabilityEqualsSource(0.7))

        network.Step()

        network.Computers[1].IsInfected |> should equal false

    /// <summary>
    /// Verifies that the MacOS infection probability is used when the second node becomes exposed.
    /// </summary>
    [<Test>]
    member _.``Mixed probabilities use MacOS probability 0.1 for node 2 on second step``() =
        let network =
            createSevenNodeLineNetwork
                0.7
                0.4
                0.1
                (ProbabilityEqualsSource(0.1))

        network.Computers[1].Infect()
        network.Step()

        network.Computers[2].IsInfected |> should equal true

    /// <summary>
    /// Verifies that the Windows infection probability is used when the third node becomes exposed.
    /// </summary>
    [<Test>]
    member _.``Mixed probabilities use Windows probability 0.7 for node 3 when exposed``() =
        let network =
            createSevenNodeLineNetwork
                0.7
                0.4
                0.1
                (ProbabilityEqualsSource(0.7))

        network.Computers[1].Infect()
        network.Computers[2].Infect()
        network.Step()

        network.Computers[3].IsInfected |> should equal true

    /// <summary>
    /// Verifies that after complete propagation with unit probabilities the network can no longer change.
    /// </summary>
    [<Test>]
    member _.``CanStillChange becomes false after full spread with all one probabilities``() =
        let network =
            createSevenNodeLineNetwork
                1.0
                1.0
                1.0
                (AlwaysInfectProbabilitySource())

        for _ in 1 .. 6 do
            network.Step()

        network.CanStillChange() |> should equal false
