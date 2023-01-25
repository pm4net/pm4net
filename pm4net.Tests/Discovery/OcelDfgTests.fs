namespace pm4net.Tests

open pm4net.Algorithms

open System
open System.IO
open Xunit

module OcelDfgTests =

    [<Fact>]
    let ``Can discover DFG from sample log`` () =
        let json = File.ReadAllText("minimal.jsonocel")
        let log = OCEL.OcelJson.deserialize json
        let dfg = log |> Discovery.Ocel.OcelDfg.Discover 0 0 0 ["customer"; "item"; "order"; "package"; "product"]
        true

    [<Fact>]
    let ``Can discover DFG from 'Github pm4py' log`` () =
        let json = File.ReadAllText("github_pm4py.jsonocel")
        let log = OCEL.OcelJson.deserialize json
        let dfg = log |> Discovery.Ocel.OcelDfg.Discover 5 5 5 ["case:concept:name"; "org:resource"; "case:repo"]
        true

    [<Fact>]
    let ``Can discover DFG from 'blazor-logs' log`` () =
        let json = File.ReadAllText("blazor-logs.jsonocel")
        let log = OCEL.OcelJson.deserialize json
        let log = log.MergeDuplicateObjects()
        let dfg = log |> Discovery.Ocel.OcelDfg.Discover 0 0 0 ["CorrelationId"; "Incremented"; "Now"; "StartDate"]
        true
