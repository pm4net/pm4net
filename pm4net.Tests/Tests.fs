namespace pm4net.Tests

open pm4net.Types
open pm4net.Algorithms
open pm4net.Visualization

open System
open System.IO
open Xunit

module Tests =

    [<Fact>]
    let ``Can discover object-centric Petri Net from sample log`` () =
        let json = File.ReadAllText("github_pm4py.jsonocel")
        let log = OCEL.OcelJson.deserialize json
        let ocpn = Discovery.Ocel.OcelPetriNet.discover log
        Assert.NotNull(ocpn)

    [<Fact>]
    let ``Can discover DFG from sample log and generate DOT graph`` () =
        let json = File.ReadAllText("github_pm4py.jsonocel")
        let log = OCEL.OcelJson.deserialize json
        let dfg = Discovery.Ocel.OcelDirectlyFollowsGraph.discoverFromLog log 5 5 5
        let dot = dfg |> Map.map (fun _ v -> Graphviz.dfg2dot v)
        dot |> Map.forall (fun _ v -> String.IsNullOrWhiteSpace v |> not) |> Assert.True

    [<Fact>]
    let ``Can create DOT graph from DFG`` () =
        let dfg = {
            Nodes = ["A", 5; "B", 3; "C", 12] |> Map.ofList
            Edges = [("A", "B"), 2; ("B", "C"), 5; ("A", "C"), 1] |> Map.ofList
        }
        let dot = dfg |> Graphviz.dfg2dot
        dot |> String.IsNullOrWhiteSpace |> Assert.False
