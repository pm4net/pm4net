namespace pm4net.Tests

open OCEL
open Xunit
open System.IO
open pm4net.Types
open pm4net.Algorithms.Layout

module ``Stable graph layout tests`` =

    let filter = {
        MinEvents = 0
        MinOccurrences = 0
        MinSuccessions = 0
        Timeframe = None
    }

    [<Fact>]
    let ``Can discover global ranking from Blazor log`` () =
        let json = File.ReadAllText("blazor-logs.jsonocel")
        let log = OcelJson.deserialize true json
        let log = log.MergeDuplicateObjects()
        let globalRanking = StableGraphLayout.ComputeGlobalRanking log
        globalRanking |> Assert.NotNull

    [<Fact>]
    let ``Can discover global ranking from 'GitHub pm4py' log`` () =
        let json = File.ReadAllText("github_pm4py.jsonocel")
        let log = OcelJson.deserialize true json
        let globalRanking = StableGraphLayout.ComputeGlobalRanking log
        globalRanking |> Assert.NotNull

    [<Fact>]
    let ``Can discover global ranking from 'recruiting' log`` () =
        let json = File.ReadAllText("recruiting.jsonocel")
        let log = OcelJson.deserialize true json
        let globalRanking = StableGraphLayout.ComputeGlobalRanking log
        globalRanking |> Assert.NotNull

    [<Fact>]
    let ``Can discover graph layout from Blazor log and discovered DFG`` () =
        let json = File.ReadAllText("blazor-logs.jsonocel")
        let log = OcelJson.deserialize true json
        let log = log.MergeDuplicateObjects()

        let dfg = pm4net.Algorithms.Discovery.Ocel.OcelDfg.Discover(filter, ["CorrelationId"], log)

        let globalRanking = StableGraphLayout.ComputeGlobalRanking log
        let layout = StableGraphLayout.ComputeGraphLayout(globalRanking, dfg, true, 32, 2f, 2f, 0.5f)

        layout |> Assert.NotNull

    [<Fact>]
    let ``Can discover graph layout from 'GitHub pm4py' log and discovered DFG`` () =
        let json = File.ReadAllText("github_pm4py.jsonocel")
        let log = OcelJson.deserialize true json

        let dfg = pm4net.Algorithms.Discovery.Ocel.OcelDfg.Discover(filter, ["case:concept:name"], log)
        //let dfg = pm4net.Algorithms.Discovery.Ocel.OcelDfg.Discover(0, 0, 0, log.ObjectTypes |> Set.toList, log)

        let globalRanking = StableGraphLayout.ComputeGlobalRanking log
        let layout = StableGraphLayout.ComputeGraphLayout(globalRanking, dfg, true, 32, 2f, 2f, 0.5f)

        layout |> Assert.NotNull

    [<Fact>]
    let ``Can discover graph layout from recruiting log and discovered DFG`` () =
        let json = File.ReadAllText("recruiting.jsonocel")
        let log = OcelJson.deserialize true json

        let dfg = pm4net.Algorithms.Discovery.Ocel.OcelDfg.Discover(filter, log.ObjectTypes |> Set.toList, log)

        let globalRanking = StableGraphLayout.ComputeGlobalRanking log
        let layout = StableGraphLayout.ComputeGraphLayout(globalRanking, dfg, true, 32, 2f, 2f, 0.5f)

        layout |> Assert.NotNull
