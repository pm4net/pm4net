namespace pm4net.Tests

open OCEL
open pm4net.Visualization.Layout

open System.IO
open Xunit

module StableGraphLayoutTests =

    [<Fact>]
    let ``Can discover stable graph layout from Blazor log`` () =
        let json = File.ReadAllText("blazor-logs.jsonocel")
        let log = OcelJson.deserialize true json
        let log = log.MergeDuplicateObjects()
        let gr = StableGraphLayout.ComputeGlobalRanking log
        gr |> Assert.NotNull

    [<Fact>]
    let ``Can discover stable graph layout from 'GitHub pm4py' log`` () =
        let json = File.ReadAllText("github_pm4py.jsonocel")
        let log = OcelJson.deserialize true json
        let gr = StableGraphLayout.ComputeGlobalRanking log
        gr |> Assert.NotNull
