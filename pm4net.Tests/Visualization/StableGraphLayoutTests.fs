namespace pm4net.Tests

open OCEL
open Xunit
open System.IO
open pm4net.Algorithms.Layout

module ``Stable graph layout tests`` =

    [<Fact>]
    let ``Can discover global rank graph from Blazor log`` () =
        let json = File.ReadAllText("blazor-logs.jsonocel")
        let log = OcelJson.deserialize true json
        let log = log.MergeDuplicateObjects()
        let globalRanking = StableGraphLayout.ComputeGlobalRanking log
        globalRanking |> Assert.NotNull

    [<Fact>]
    let ``Can discover global rank graph from 'GitHub pm4py' log`` () =
        let json = File.ReadAllText("github_pm4py.jsonocel")
        let log = OcelJson.deserialize true json
        let globalRanking = StableGraphLayout.ComputeGlobalRanking log
        globalRanking |> Assert.NotNull

    [<Fact>]
    let ``Can discover global rank graph from 'recruiting' log`` () =
        let json = File.ReadAllText("recruiting.jsonocel")
        let log = OcelJson.deserialize true json
        let globalRanking = StableGraphLayout.ComputeGlobalRanking log
        globalRanking |> Assert.NotNull

    [<Fact>]
    let ``Can discover global order from Blazor log and discovered DFG`` () =
        let json = File.ReadAllText("blazor-logs.jsonocel")
        let log = OcelJson.deserialize true json
        let log = log.MergeDuplicateObjects()

        let dfg = pm4net.Algorithms.Discovery.Ocel.OcelDfg.Discover(0, 0, 0, ["CorrelationId"], log)

        let globalRanking = StableGraphLayout.ComputeGlobalRanking log
        let layout = StableGraphLayout.ComputeGraphLayout(globalRanking, dfg, true, 32, 2f, 2f, 0.5f)

        layout |> Assert.NotNull

    [<Fact>]
    let ``Can discover global order from 'GitHub pm4py' log and discovered DFG`` () =
        let json = File.ReadAllText("github_pm4py.jsonocel")
        let log = OcelJson.deserialize true json

        let dfg = pm4net.Algorithms.Discovery.Ocel.OcelDfg.Discover(0, 0, 0, ["case:concept:name"], log)
        //let dfg = pm4net.Algorithms.Discovery.Ocel.OcelDfg.Discover(0, 0, 0, log.ObjectTypes |> Set.toList, log)

        let globalRanking = StableGraphLayout.ComputeGlobalRanking log
        let layout = StableGraphLayout.ComputeGraphLayout(globalRanking, dfg, true, 32, 2f, 2f, 0.5f)

        layout |> Assert.NotNull
