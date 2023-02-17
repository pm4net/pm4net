namespace pm4net.Tests

open OCEL
open pm4net.Types.Dfg
open pm4net.Visualization.Layout

open System.Collections.Generic
open System.IO
open Xunit

module StableGraphLayoutTests =

    /// Tests whether the global ranking graph is valid
    let globalRankValid (gr: GlobalRankGraph) =
        gr.Edges |> List.forall (fun ((a, aRank), (b, bRank), _) -> if a = b then aRank = bRank else aRank <> bRank) // No horizontal edges (except self-loops)

    /// Tests whether there are any duplicate nodes
    let noDuplicateNodes (gr: GlobalRankGraph) =
        gr.Nodes |> List.distinctBy fst |> List.length = gr.Nodes.Length

    /// Tests whether there are any duplicate edges
    let noDuplicateEdges (gr: GlobalRankGraph) =
        gr.Edges |> List.distinctBy (fun ((a, _), (b, _), _) -> a, b) |> List.length = gr.Edges.Length

    /// Asserts the properties of a global rank graph
    let assertGlobalRank gr =
        Assert.True(globalRankValid gr, "Global rank has horizontal edges")
        Assert.True(noDuplicateNodes gr, "Global rank has duplicate nodes")
        Assert.True(noDuplicateEdges gr, "Global rank has duplicate edges")

    /// Assert all global rank graphs in a map
    let assertGraphMap gr =
        Assert.All(gr, System.Action<KeyValuePair<_, _>>(fun kv -> kv.Value |> fst |> assertGlobalRank))

    [<Fact>]
    let ``Can discover stable graph layout from Blazor log for each object type`` () =
        let json = File.ReadAllText("blazor-logs.jsonocel")
        let log = OcelJson.deserialize true json
        let log = log.MergeDuplicateObjects()
        let gr = StableGraphLayout.ComputeGlobalRankingForEachObjectType log
        assertGraphMap gr

    [<Fact>]
    let ``Can discover stable graph layout from Blazor log`` () =
        let json = File.ReadAllText("blazor-logs.jsonocel")
        let log = OcelJson.deserialize true json
        let log = log.MergeDuplicateObjects()
        let gr, skeleton = StableGraphLayout.ComputeGlobalRanking log
        assertGlobalRank gr

    [<Fact>]
    let ``Can discover stable graph layout from 'GitHub pm4py' log for each object type`` () =
        let json = File.ReadAllText("github_pm4py.jsonocel")
        let log = OcelJson.deserialize true json
        let gr = StableGraphLayout.ComputeGlobalRankingForEachObjectType log
        assertGraphMap gr

    [<Fact>]
    let ``Can discover stable graph layout from 'GitHub pm4py' log`` () =
        let json = File.ReadAllText("github_pm4py.jsonocel")
        let log = OcelJson.deserialize true json
        let gr, skeleton = StableGraphLayout.ComputeGlobalRanking log
        assertGlobalRank gr

    [<Fact>]
    let ``Can discover stable graph layout from 'recruiting' log for each object type`` () =
        let json = File.ReadAllText("recruiting.jsonocel")
        let log = OcelJson.deserialize true json
        let gr = StableGraphLayout.ComputeGlobalRankingForEachObjectType log
        assertGraphMap gr

    [<Fact>]
    let ``Can discover stable graph layout from 'recruiting' log`` () =
        let json = File.ReadAllText("recruiting.jsonocel")
        let log = OcelJson.deserialize true json
        let gr, skeleton = StableGraphLayout.ComputeGlobalRanking log
        assertGlobalRank gr
