namespace pm4net.Tests

open pm4net.Types
open pm4net.Visualization

open System
open Xunit

module GraphvizTests =

    [<Fact>]
    let ``Can create DOT graph from DFG`` () =
        let dfg : DirectedGraph<string, int> = {
            Nodes = ["A", 5; "B", 3; "C", 12] |> Map.ofList
            Edges = [("A", "B"), 2; ("B", "C"), 5; ("A", "C"), 1] |> Map.ofList
        }
        let dot = dfg |> Graphviz.dfg2dot
        dot |> String.IsNullOrWhiteSpace |> Assert.False
