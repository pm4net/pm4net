namespace pm4net.Visualization

open pm4net.Types
open pm4net.Types.Dfg
open DotNetGraph
open DotNetGraph.Node
open DotNetGraph.Edge
open DotNetGraph.SubGraph
open DotNetGraph.Attributes
open DotNetGraph.Extensions

module Graphviz =

    /// Convert an Object-Centric Directly-Follows-Graph (OC-DFG) into a DOT graph
    let ocdfg2dot (ocdfg: DirectedGraph<Node, Edge>) =

        let nodeName = function
            | EventNode n -> n.Name
            | StartNode n -> $"{nameof(StartNode)} {n}"
            | EndNode n -> $"{nameof(EndNode)} {n}"

        let graph = DotGraph("DFG", true)

        let eventNodesByNameSpace =
            ocdfg.Nodes
            |> List.choose (fun n -> match n with | EventNode n -> Some n | _ -> None)
            |> List.groupBy (fun n -> n.Namespace)

        let namespaceSubGraphs =
            eventNodesByNameSpace
            |> List.mapi (fun i (ns, nodes) ->
                let subGraph = DotSubGraph($"cluster_{i}")
                nodes
                |> List.map (fun n ->
                    let node = DotNode($"{n.Name}_{ns}")
                    node.SetCustomAttribute("label", $"<<B>{n.Name}</B><BR/>{n.Statistics.Frequency}>") |> ignore
                    node.Shape <- DotNodeShapeAttribute DotNodeShape.Rectangle
                    node.Style <- DotNodeStyleAttribute DotNodeStyle.Filled
                    node.FillColor <-
                        match n.Level with
                        | Debug | Verbose -> DotFillColorAttribute System.Drawing.Color.White
                        | Information -> DotFillColorAttribute System.Drawing.Color.LightGray
                        | Warning -> DotFillColorAttribute System.Drawing.Color.Orange
                        | Error -> DotFillColorAttribute System.Drawing.Color.Red
                        | Fatal -> DotFillColorAttribute System.Drawing.Color.DarkRed
                        | _ -> DotFillColorAttribute System.Drawing.Color.White
                    node.FontColor <-
                        match n.Level with
                        | Debug | Verbose -> DotFontColorAttribute System.Drawing.Color.Black
                        | Information -> DotFontColorAttribute System.Drawing.Color.Black
                        | Warning -> DotFontColorAttribute System.Drawing.Color.Black
                        | Error -> DotFontColorAttribute System.Drawing.Color.White
                        | Fatal -> DotFontColorAttribute System.Drawing.Color.White
                        | _ -> DotFontColorAttribute System.Drawing.Color.Black
                    node
                )
                |> List.iter (fun n -> subGraph.Elements.Add n)
                subGraph.Label <- ns
                subGraph
            )
        namespaceSubGraphs |> List.iter (fun n -> graph.Elements.Add n)

        let startEndNodes =
            ocdfg.Nodes
            |> List.choose (fun n ->
                match n with
                | StartNode n ->
                    let node = DotNode(StartNode n |> nodeName)
                    node.Label <- n
                    node.Shape <- DotNodeShapeAttribute DotNodeShape.Ellipse
                    Some node
                | EndNode n ->
                    let node = DotNode(EndNode n |> nodeName)
                    node.Label <- n
                    node.Shape <- DotNodeShapeAttribute DotNodeShape.Underline
                    Some node
                | _ -> None)
        startEndNodes |> List.iter (fun n -> graph.Elements.Add n)

        let edges =
            ocdfg.Edges
            |> List.map (fun (a, b, e) ->
                let edge = DotEdge(nodeName a, nodeName b)
                edge.Label <- e.Statistics.Frequency.ToString()
                edge
            )
        edges |> List.iter (fun e -> graph.Elements.Add e)

        graph.Compile(true)
