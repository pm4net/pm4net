namespace pm4net.Visualization

open pm4net.Types
open DotNetGraph
open DotNetGraph.Node
open DotNetGraph.Edge
open DotNetGraph.Attributes
open DotNetGraph.Extensions

module Graphviz =

    /// Convert a Directly-Follows Graph to a graph in the DOT language
    let dfg2dot (dfg: DirectedGraph<string, int>) =
        let graph = DotGraph("DFG", true)

        let nodes =
            dfg.Nodes
            |> Map.toList
            |> List.map (fun (name, freq) ->
                let node = DotNode(name)
                // Using custom attribute because standard Label always adds quotes, which is not wanted when using HTML labels (https://graphviz.org/doc/info/shapes.html#html)
                node.SetCustomAttribute("label", $"<<B>{name}</B><BR/>{freq}>") |> ignore
                node.Shape <- DotNodeShapeAttribute DotNodeShape.Rectangle
                node
            )
        nodes |> List.iter (fun n -> graph.Elements.Add n)

        let edges =
            dfg.Edges
            |> Map.toList
            |> List.map (fun ((dep, arr), freq) ->
                let edge = DotEdge(dep, arr)
                edge.Label <- freq.ToString()
                edge
            )
        edges |> List.iter (fun e -> graph.Elements.Add e)
        
        graph.Compile(true)
