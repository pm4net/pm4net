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
        let dfg = Discovery.Ocel.OcelDirectlyFollowsGraph.discoverFromLog log 0 0 0 false
        let dot = dfg |> Map.map (fun _ v -> Graphviz.dfg2dot v)
        dot |> Map.forall (fun _ v -> String.IsNullOrWhiteSpace v |> not) |> Assert.True

    [<Fact>]
    let ``Can discover DFG from 'Github pm4py' log and generate DOT graph`` () =
        let json = File.ReadAllText("github_pm4py.jsonocel")
        let log = OCEL.OcelJson.deserialize json
        let dfg = Discovery.Ocel.OcelDirectlyFollowsGraph.discoverFromLog log 5 5 5 false
        let dot = dfg |> Map.map (fun _ v -> Graphviz.dfg2dot v)
        dot |> Map.forall (fun _ v -> String.IsNullOrWhiteSpace v |> not) |> Assert.True

    [<Fact>]
    let ``Can discover DFG from 'blazor-logs' log and generate DOT graph`` () =
        let json = File.ReadAllText("blazor-logs.jsonocel")
        let log = OCEL.OcelJson.deserialize json
        let dfg = Discovery.Ocel.OcelDirectlyFollowsGraph.discoverFromLog log 0 0 0 true
        let dot = dfg |> Map.map (fun _ v -> Graphviz.dfg2dot v)
        dot |> Map.forall (fun _ v -> String.IsNullOrWhiteSpace v |> not) |> Assert.True
