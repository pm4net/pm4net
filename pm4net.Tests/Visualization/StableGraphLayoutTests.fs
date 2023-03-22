namespace pm4net.Tests

open OCEL
open Xunit
open System.IO
open System.Collections.Generic
open pm4net.Types.GraphLayout
open pm4net.Algorithms.Layout
open pm4net.Visualization.Layout

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
    let ``Can discover global rank graph from Blazor log`` () =
        let json = File.ReadAllText("blazor-logs.jsonocel")
        let log = OcelJson.deserialize true json
        let log = log.MergeDuplicateObjects()
        let (rankGraph, skeleton, components) = StableGraphLayout.ComputeRankGraph log
        let dot = LayoutStepsVisualizer.globalRankGraphToDot rankGraph
        rankGraph |> Assert.NotNull

    [<Fact>]
    let ``Can discover global rank graph from 'GitHub pm4py' log`` () =
        let json = File.ReadAllText("github_pm4py.jsonocel")
        let log = OcelJson.deserialize true json
        let (rankGraph, skeleton, components) = StableGraphLayout.ComputeRankGraph log
        let dot = LayoutStepsVisualizer.globalRankGraphToDot rankGraph
        rankGraph |> Assert.NotNull

    [<Fact>]
    let ``Can discover global rank graph from 'recruiting' log`` () =
        let json = File.ReadAllText("recruiting.jsonocel")
        let log = OcelJson.deserialize true json
        let (rankGraph, skeleton, components) = StableGraphLayout.ComputeRankGraph log
        let dot = LayoutStepsVisualizer.globalRankGraphToDot rankGraph
        rankGraph |> Assert.NotNull

    [<Fact>]
    let ``Can discover global order from Blazor log and discovered DFG`` () =
        let json = File.ReadAllText("blazor-logs.jsonocel")
        let log = OcelJson.deserialize true json
        let log = log.MergeDuplicateObjects()
        let (rankGraph, skeleton, components) = StableGraphLayout.ComputeRankGraph log
        let dfg = pm4net.Algorithms.Discovery.Ocel.OcelDfg.Discover(0, 0, 0, ["CorrelationId"], log)

        // For printing NSG with DOT (only works with neato or fdp layout)
        let (rankGraph, _) =  (rankGraph, components, dfg) |||> GraphLayoutAlgo.fixHorizontalEdgesInGlobalRankGraphForDiscoveredModel
        let nsg = (rankGraph, skeleton) ||> GraphLayoutAlgo.computeNodeSequenceGraph
        let dotRg = LayoutStepsVisualizer.globalRankGraphToDot rankGraph
        let dotNsg = LayoutStepsVisualizer.nodeSequenceGraphToDot nsg
        let goNsg = (rankGraph, skeleton) ||> StableGraphLayout.computeGlobalRanking
        let dotGoNsg = LayoutStepsVisualizer.nodeSequenceGraphToDot goNsg

        let discoveredGraph = StableGraphLayout.ComputeGlobalOrder(rankGraph, skeleton, components, dfg, true, 2, 10, 1f, 2f, 0.5f)
        let discDot = LayoutStepsVisualizer.discoveredGraphToDot discoveredGraph

        //let crossMinNsgDot = LayoutStepsVisualizer.crossMinGraphToDot globalOrder
        discoveredGraph |> Assert.NotNull

    [<Fact>]
    let ``Can discover global order from 'GitHub pm4py' log and discovered DFG`` () =
        let json = File.ReadAllText("github_pm4py.jsonocel")
        let log = OcelJson.deserialize true json
        //let dfg = pm4net.Algorithms.Discovery.Ocel.OcelDfg.Discover(0, 0, 0, ["case:concept:name"], log)
        let dfg = pm4net.Algorithms.Discovery.Ocel.OcelDfg.Discover(0, 0, 0, log.ObjectTypes |> Set.toList, log)
        let (rankGraph, skeleton, components) = StableGraphLayout.ComputeRankGraph log

        // For printing NSG with DOT (only works with neato or fdp layout)
        let (rankGraph, _) =  (rankGraph, components, dfg) |||> GraphLayoutAlgo.fixHorizontalEdgesInGlobalRankGraphForDiscoveredModel
        let nsg = (rankGraph, skeleton) ||> GraphLayoutAlgo.computeNodeSequenceGraph
        let dotNsg = LayoutStepsVisualizer.nodeSequenceGraphToDot nsg
        let goNsg = (rankGraph, skeleton) ||> StableGraphLayout.computeGlobalRanking
        let dotGoNsg = LayoutStepsVisualizer.nodeSequenceGraphToDot goNsg

        let discoveredGraph = StableGraphLayout.ComputeGlobalOrder(rankGraph, skeleton, components, dfg, true, 2, 10, 1f, 2f, 0.5f)
        let discDot = LayoutStepsVisualizer.discoveredGraphToDot discoveredGraph

        //let crossMinNsgDot = LayoutStepsVisualizer.crossMinGraphToDot globalOrder
        discoveredGraph |> Assert.NotNull
