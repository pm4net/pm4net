namespace pm4net.Tests

open pm4net.Types
open pm4net.Algorithms
open pm4net.Visualization

open System
open System.IO
open Xunit

module VisualizationPipelineTests =

    [<Fact>]
    let ``Can discover DFG from sample log and generate DOT graph`` () =
        let json = File.ReadAllText("minimal.jsonocel")
        let log = OCEL.OcelJson.deserialize json
        let dfg = log |> Discovery.Ocel.OcelDfg.Discover 0 0 0 ["customer"; "item"; "order"; "package"; "product"]
        let dot = dfg |> Graphviz.ocdfg2dot
        dot |> String.IsNullOrWhiteSpace |> Assert.False

    [<Fact>]
    let ``Can discover DFG from 'Github pm4py' log and generate DOT graph`` () =
        let json = File.ReadAllText("github_pm4py.jsonocel")
        let log = OCEL.OcelJson.deserialize json
        let dfg = log |> Discovery.Ocel.OcelDfg.Discover 5 5 5 ["case:concept:name"]
        let dot = dfg |> Graphviz.ocdfg2dot
        dot |> String.IsNullOrWhiteSpace |> Assert.False

    [<Fact>]
    let ``Can discover DFG from 'blazor-logs' log and generate DOT graph`` () =
        let json = File.ReadAllText("blazor-logs.jsonocel")
        let log = OCEL.OcelJson.deserialize json
        let log = log.MergeDuplicateObjects()
        let dfg = log |> Discovery.Ocel.OcelDfg.Discover 0 0 0 ["CorrelationId"; "StartDate"]
        let dot = dfg |> Graphviz.ocdfg2dot
        dot |> String.IsNullOrWhiteSpace |> Assert.False