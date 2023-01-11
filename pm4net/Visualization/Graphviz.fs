namespace pm4net.Visualization

open pm4net.Types
open DotNetGraph
open DotNetGraph.Node
open DotNetGraph.Edge
open DotNetGraph.Attributes
open DotNetGraph.Extensions

module Graphviz =

    /// Convert an Object-Centric Directly-Follows-Graph (OC-DFG) into a DOT graph
    let ocdfg2dot (ocdfg: DirectedGraph<DfgNode, DfgEdge>) =

        let nodeName = function
            | EventNode n -> n.Name
            | StartNode n -> $"{nameof(StartNode)} {n}"
            | EndNode n -> $"{nameof(EndNode)} {n}"

        let graph = DotGraph("DFG", true)

        let nodes =
            ocdfg.Nodes
            |> List.map (fun n ->
                match n with
                | EventNode n ->
                    let node = DotNode(n.Name)
                    node.SetCustomAttribute("label", $"<<B>{n.Name}</B><BR/>{n.Frequency}>") |> ignore
                    node.Shape <- DotNodeShapeAttribute DotNodeShape.Rectangle
                    node
                | StartNode n ->
                    let node = DotNode(StartNode n |> nodeName)
                    node.Label <- n
                    node.Shape <- DotNodeShapeAttribute DotNodeShape.Ellipse
                    node
                | EndNode n ->
                    let node = DotNode(EndNode n |> nodeName)
                    node.Label <- n
                    node.Shape <- DotNodeShapeAttribute DotNodeShape.Underline
                    node
            )
        nodes |> List.iter (fun n -> graph.Elements.Add n)

        let edges =
            ocdfg.Edges
            |> List.map (fun (a, b, (name, freq)) ->
                let edge = DotEdge(nodeName a, nodeName b)
                edge.Label <- freq.ToString()
                edge
            )
        edges |> List.iter (fun e -> graph.Elements.Add e)

        graph.Compile(true)
