namespace pm4net.Tests

open OCEL
open pm4net.Types.Dfg
open pm4net.Visualization.Layout

open System.IO
open Xunit

module StableGraphLayoutTests =

    /// Tests whether the global ranking graph is valid
    let globalRankValid (gr: DirectedGraph<string * int, int>) =
        gr.Edges |> List.forall (fun ((a, aRank), (b, bRank), _) -> if a = b then aRank = bRank else aRank <> bRank) // No horizontal edges (except self-loops)

    [<Fact>]
    let ``Can discover stable graph layout from Blazor log`` () =
        let json = File.ReadAllText("blazor-logs.jsonocel")
        let log = OcelJson.deserialize true json
        let log = log.MergeDuplicateObjects()
        let gr = StableGraphLayout.ComputeGlobalRanking log
        gr |> Map.forall (fun _ v -> globalRankValid v) |> Assert.True

    [<Fact>]
    let ``Can discover stable graph layout from 'GitHub pm4py' log`` () =
        let json = File.ReadAllText("github_pm4py.jsonocel")
        let log = OcelJson.deserialize true json
        let gr = StableGraphLayout.ComputeGlobalRanking log
        gr |> Map.forall (fun _ v -> globalRankValid v) |> Assert.True

    [<Fact>]
    let ``Can discover stable graph layout from 'recruiting' log`` () =
        let json = File.ReadAllText("recruiting.jsonocel")
        let log = OcelJson.deserialize true json
        let gr = StableGraphLayout.ComputeGlobalRanking log
        gr |> Map.forall (fun _ v -> globalRankValid v) |> Assert.True
