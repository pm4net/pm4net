namespace pm4net.Visualization

open System
open pm4net.Types
open pm4net.Types.Dfg
open pm4net.Types.Trees
open DotNetGraph
open DotNetGraph.Node
open DotNetGraph.Edge
open DotNetGraph.SubGraph
open DotNetGraph.Attributes
open DotNetGraph.Extensions

module Graphviz =

    /// Extract a tree hierarchy from a list of fully qualified namespaces
    let private namespaceTree separators (namespaces: string list) : ListTree<string> =

        /// Insert a list of sequential values into a tree
        let rec insert (tree: ListTree<'a>) (values: 'a list) : ListTree<'a> =

            /// Get the node and its index in a list of nodes that has a given value, if any exist.
            let hasExistingNodeIndex nodes value =
                match nodes |> List.tryFindIndex (fun (Node(v, _)) -> v = value) with
                | Some i -> Some (nodes[i], i)
                | None -> None

            // Only has the Node type, but that has to be deconstructed first
            match tree with
            | Node(node, children) ->
                // Return the input tree if there are no values left to add.
                // Otherwise recurisvely add each item, stepping one level down into the tree with each value.
                match values with
                | [] -> tree
                | head :: tail ->
                    // Check whether there is already a child with the given value.
                    // If yes, discard the duplicate value and continue with the next value on the existing branch.
                    // If no, add the new value as a child of the current node, with no children of itself, and continue the recursive pattern.
                    match head |> hasExistingNodeIndex children with
                    | Some (n, i) ->
                        let updatedNode = insert n tail
                        Node(node, children |> List.updateAt i updatedNode)
                    | None ->
                        // Create a new child node by recursively adding the remaining values, and then adding it to the existing node.
                        let newNode = insert (Node(head, [])) tail
                        Node(node, newNode :: children)

        // Fold over the different namespaces and build up the tree one after another, starting with a root node with an empty string.
        (Node(String.Empty, []), namespaces)
        ||> List.fold (fun tree ns ->
            (ns.Split(separators)
                |> Array.filter (fun i -> i <> String.Empty)
                |> List.ofArray)
            |> insert tree)

    /// Convert an Object-Centric Directly-Follows-Graph (OC-DFG) into a DOT graph
    let ocdfg2dot (ocdfg: DirectedGraph<Node, Edge>) =

        let nodeName = function
            | EventNode n -> n.Name
            | StartNode n -> $"{nameof(StartNode)} {n}"
            | EndNode n -> $"{nameof(EndNode)} {n}"

        let graph = DotGraph("DFG", true)

        let namespaces =
            ocdfg.Nodes
            |> List.choose (fun n -> match n with | EventNode n -> Some n | _ -> None)
            |> List.map (fun n -> n.Namespace)
            |> List.distinct

        let namespaceTree = namespaceTree [|'.'|] namespaces

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
