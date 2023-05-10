namespace pm4net.Tests

open pm4net.Types
open pm4net.Algorithms
open pm4net.Visualization.Ocel

open System
open System.IO
open Xunit

module VisualizationPipelineTests =

    let filter = {
        MinEvents = 0
        MinOccurrences = 0
        MinSuccessions = 0
        Timeframe = None
    }

    [<Fact>]
    let ``Can discover DFG from sample log and generate DOT graph`` () =
        let json = File.ReadAllText("minimal.jsonocel")
        let log = OCEL.OcelJson.deserialize true json
        let dfg = Discovery.Ocel.OcelDfg.Discover(filter, ["customer"; "item"; "order"; "package"; "product"], log)
        let dot = Graphviz.OcDfg2Dot dfg false
        dot |> String.IsNullOrWhiteSpace |> Assert.False

    [<Fact>]
    let ``Can discover DFG from 'Github pm4py' log and generate DOT graph`` () =
        let json = File.ReadAllText("github_pm4py.jsonocel")
        let log = OCEL.OcelJson.deserialize true json
        let dfg = Discovery.Ocel.OcelDfg.Discover(filter, ["case:concept:name"], log)
        let dot = Graphviz.OcDfg2Dot dfg false
        dot |> String.IsNullOrWhiteSpace |> Assert.False

    [<Fact>]
    let ``Can discover DFG from 'blazor-logs' log and generate DOT graph`` () =
        let json = File.ReadAllText("blazor-logs.jsonocel")
        let log = OCEL.OcelJson.deserialize true json
        let log = log.MergeDuplicateObjects()
        let dfg = Discovery.Ocel.OcelDfg.Discover(filter, ["CorrelationId"; "StartDate"; "Now"; "Incremented"], log)
        let dot = Graphviz.OcDfg2Dot dfg false
        dot |> String.IsNullOrWhiteSpace |> Assert.False

    [<Fact>]
    let ``Can discover DFG from 'blazor-logs' log and generate DOT graph with namespace clustering`` () =
        let json = File.ReadAllText("blazor-logs.jsonocel")
        let log = OCEL.OcelJson.deserialize true json
        let log = log.MergeDuplicateObjects()
        let dfg = Discovery.Ocel.OcelDfg.Discover(filter, ["CorrelationId"; "StartDate"; "Now"; "Incremented"], log)
        let dot = Graphviz.OcDfg2Dot dfg true
        dot |> String.IsNullOrWhiteSpace |> Assert.False
