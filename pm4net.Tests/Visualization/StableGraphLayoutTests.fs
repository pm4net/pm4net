namespace pm4net.Tests

open OCEL
open pm4net.Visualization.Layout

open System.IO
open Xunit

module StableGraphLayoutTests =

    [<Fact>]
    let ``Can discover stable graph layout from sample log`` () =
        let json = File.ReadAllText("github_pm4py.jsonocel")
        let log = OcelJson.deserialize true json
        let gr = StableGraphLayout.ComputeGlobalRanking log
        true
