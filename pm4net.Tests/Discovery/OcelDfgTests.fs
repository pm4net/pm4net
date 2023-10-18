namespace pm4net.Tests

open pm4net.Types
open pm4net.Types.Trees
open pm4net.Algorithms

open System
open System.IO
open Xunit

module OcelDfgTests =

    let filter = {
        MinEvents = 0
        MinOccurrences = 0
        MinSuccessions = 0
        Timeframe = None
        IncludedLogLevels = [ LogLevel.Unknown; LogLevel.Verbose; LogLevel.Debug; LogLevel.Information; LogLevel.Warning; LogLevel.Error; LogLevel.Fatal ]
        IncludedNamespaces = None
    }

    let dataPath = @"..\..\..\..\..\..\data\pm4net\"

    let timeframeFilter = {
        filter with
            Timeframe = Some {
                From = DateTimeOffset.Parse("2018-01-01")
                To = DateTimeOffset.Parse("2020-01-01")
                KeepCases = ContainedInTimeframe
            }
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

    [<Fact>]
    let ``Can discover DFG from sample log`` () =
        let json = File.ReadAllText(Path.Combine(dataPath, "minimal.jsonocel"))
        let log = OCEL.OcelJson.deserialize true json
        let dfg = Discovery.Ocel.OcelDfg.Discover(filter, ["customer"; "item"; "order"; "package"; "product"], log)
        true

    [<Fact>]
    let ``Can discover DFG from 'Github pm4py' log`` () =
        let json = File.ReadAllText(Path.Combine(dataPath, "github_pm4py.jsonocel"))
        let log = OCEL.OcelJson.deserialize true json
        let dfg = Discovery.Ocel.OcelDfg.Discover(filter, ["case:concept:name"; "org:resource"; "case:repo"], log)
        true

    [<Fact>]
    let ``Can discover DFG from 'Github pm4py' log with timeframe filter`` () =
        let json = File.ReadAllText(Path.Combine(dataPath, "github_pm4py.jsonocel"))
        let log = OCEL.OcelJson.deserialize true json
        let dfg = Discovery.Ocel.OcelDfg.Discover(timeframeFilter, log.ObjectTypes |> Set.toList, log)
        true

    [<Fact>]
    let ``Can discover DFG from 'blazor-logs' log`` () =
        let json = File.ReadAllText(Path.Combine(dataPath, "blazor-logs.jsonocel"))
        let log = OCEL.OcelJson.deserialize true json
        let log = log.MergeDuplicateObjects()
        let dfg = Discovery.Ocel.OcelDfg.Discover(filter, ["CorrelationId"; "Incremented"; "Now"; "StartDate"], log)
        true

    [<Fact>]
    let ``Can discover DFG from 'blazor-logs' log with namespace filter`` () =
        let json = File.ReadAllText(Path.Combine(dataPath, "blazor-logs.jsonocel"))
        let log = OCEL.OcelJson.deserialize true json
        let log = log.MergeDuplicateObjects()
        let dfg = Discovery.Ocel.OcelDfg.Discover(namespaceFilter, ["CorrelationId"; "Incremented"; "Now"; "StartDate"], log)
        true

    [<Fact>]
    let ``Can discover DFG from 'recruiting' log`` () =
        let json = File.ReadAllText(Path.Combine(dataPath, "recruiting.jsonocel"))
        let log = OCEL.OcelJson.deserialize true json
        let dfg = Discovery.Ocel.OcelDfg.Discover(filter, log.ObjectTypes |> Set.toList, log)
        true

    [<Fact>]
    let ``Can discover DFG from 'windows_events' log`` () =
        let json = File.ReadAllText(Path.Combine(dataPath, "windows_events.jsonocel"))
        let log = OCEL.OcelJson.deserialize true json
        let dfg = Discovery.Ocel.OcelDfg.Discover(filter, log.ObjectTypes |> Set.toList, log)
        true
