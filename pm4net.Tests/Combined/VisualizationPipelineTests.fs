namespace pm4net.Tests

open pm4net.Types
open pm4net.Types.Trees
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
        IncludedLogLevels = [ LogLevel.Unknown; LogLevel.Verbose; LogLevel.Debug; LogLevel.Information; LogLevel.Warning; LogLevel.Error; LogLevel.Fatal ]
        IncludedNamespaces = None
    }

    let namespaceFilter = {
        filter with
            IncludedNamespaces = ListTree.Node("", [
                ListTree.Node("BlazorExample", [
                    ListTree.Node("Pages", [
                        ListTree.Node("*", [])
                    ])
                ])
            ]) |> Some
    }

    let dataPath = @"..\..\..\..\..\..\data\pm4net\"

    module Dot =

        [<Fact>]
        let ``Can discover DFG from sample log and generate DOT graph`` () =
            let json = File.ReadAllText(Path.Combine(dataPath, "minimal.jsonocel"))
            let log = OCEL.OcelJson.deserialize true json
            let dfg = Discovery.Ocel.OcelDfg.Discover(filter, ["customer"; "item"; "order"; "package"; "product"], log)
            let dot = Graphviz.OcDfg2Dot dfg false
            dot |> String.IsNullOrWhiteSpace |> Assert.False

        [<Fact>]
        let ``Can discover DFG from 'Github pm4py' log and generate DOT graph`` () =
            let json = File.ReadAllText(Path.Combine(dataPath, "github_pm4py.jsonocel"))
            let log = OCEL.OcelJson.deserialize true json
            let dfg = Discovery.Ocel.OcelDfg.Discover(filter, ["case:concept:name"], log)
            let dot = Graphviz.OcDfg2Dot dfg false
            dot |> String.IsNullOrWhiteSpace |> Assert.False

        [<Fact>]
        let ``Can discover DFG from 'blazor-logs' log and generate DOT graph`` () =
            let json = File.ReadAllText(Path.Combine(dataPath, "blazor-logs.jsonocel"))
            let log = OCEL.OcelJson.deserialize true json
            let log = log.MergeDuplicateObjects()
            let dfg = Discovery.Ocel.OcelDfg.Discover(filter, ["CorrelationId"; "StartDate"; "Now"; "Incremented"], log)
            let dot = Graphviz.OcDfg2Dot dfg false
            dot |> String.IsNullOrWhiteSpace |> Assert.False

        [<Fact>]
        let ``Can discover DFG from 'blazor-logs' log with namespace filter and generate DOT graph`` () =
            let json = File.ReadAllText(Path.Combine(dataPath, "blazor-logs.jsonocel"))
            let log = OCEL.OcelJson.deserialize true json
            let log = log.MergeDuplicateObjects()
            let dfg = Discovery.Ocel.OcelDfg.Discover(namespaceFilter, ["CorrelationId"; "StartDate"; "Now"; "Incremented"], log)
            let dot = Graphviz.OcDfg2Dot dfg false
            dot |> String.IsNullOrWhiteSpace |> Assert.False

        [<Fact>]
        let ``Can discover DFG from 'blazor-logs' log and generate DOT graph with namespace clustering`` () =
            let json = File.ReadAllText(Path.Combine(dataPath, "blazor-logs.jsonocel"))
            let log = OCEL.OcelJson.deserialize true json
            let log = log.MergeDuplicateObjects()
            let dfg = Discovery.Ocel.OcelDfg.Discover(filter, ["CorrelationId"; "StartDate"; "Now"; "Incremented"], log)
            let dot = Graphviz.OcDfg2Dot dfg true
            dot |> String.IsNullOrWhiteSpace |> Assert.False

    module Msagl =

        [<Fact>]
        let ``Can discover DFG from 'Github pm4py' log and generate MSAGL graph`` () =
            let json = File.ReadAllText(Path.Combine(dataPath, "github_pm4py.jsonocel"))
            let log = OCEL.OcelJson.deserialize true json
            let log = log.MergeDuplicateObjects()
            let dfg = Discovery.Ocel.OcelDfg.Discover(filter, log.ObjectTypes |> Set.toList, log)
            let svg = Msagl.OcDfg2Msagl dfg false
            svg |> String.IsNullOrWhiteSpace |> Assert.False
