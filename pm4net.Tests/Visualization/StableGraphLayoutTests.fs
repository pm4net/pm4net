namespace pm4net.Tests

open OCEL
open Xunit
open System.IO
open System.Collections.Generic
open pm4net.Types.GraphLayout
open pm4net.Algorithms.Layout

module Assertions =

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

module ``Stable graph layout tests`` =

    [<Fact>]
    let ``Can discover global order from Blazor log`` () =
        let json = File.ReadAllText("blazor-logs.jsonocel")
        let log = OcelJson.deserialize true json
        let log = log.MergeDuplicateObjects()
        let globalOrder = StableGraphLayout.ComputeGlobalOrder log
        globalOrder |> Assert.NotNull

    [<Fact>]
    let ``Can discover global order from 'GitHub pm4py' log`` () =
        let json = File.ReadAllText("github_pm4py.jsonocel")
        let log = OcelJson.deserialize true json
        let globalOrder = StableGraphLayout.ComputeGlobalOrder log
        globalOrder |> Assert.NotNull

    [<Fact>]
    let ``Can discover global order from 'recruiting' log`` () =
        let json = File.ReadAllText("recruiting.jsonocel")
        let log = OcelJson.deserialize true json
        let globalOrder = StableGraphLayout.ComputeGlobalOrder log
        globalOrder |> Assert.NotNull

    [<Fact>]
    let ``Can discover global order from 'GitHub pm4py' log and discovered DFG`` () =
        let json = File.ReadAllText("github_pm4py.jsonocel")
        let log = OcelJson.deserialize true json
        let dfg = pm4net.Algorithms.Discovery.Ocel.OcelDfg.Discover(0, 0, 0, log.ObjectTypes |> Set.toList, log)
        let globalOrder = StableGraphLayout.ComputeGlobalOrder(log, dfg)
        globalOrder |> Assert.NotNull
