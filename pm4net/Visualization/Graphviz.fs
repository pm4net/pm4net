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
        let rec insert tree values =

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

    // Add empty sub-graphs with the corresponding ID's to each node in a tree
    let rec private addSubGraphs (tree: ListTree<string>) : ListTree<string * DotSubGraph> =
        match tree with
        | Node(node, children) ->
            // Unique ID to avoid duplicate ID's when single parts of namespaces exist multiple times in different paths
            let id = Guid.NewGuid().ToString("N")
            let subGraph = DotSubGraph($"cluster_{id}")
            subGraph.Label <- node
            Node((node, subGraph), children |> List.map addSubGraphs)

    /// Convert an Object-Centric Directly-Follows-Graph (OC-DFG) into a DOT graph
    let ocdfg2dot (ocdfg: DirectedGraph<Node, Edge>) =

        /// Get a unique name for a node
        let nodeName = function
            | EventNode n -> n.Name
            | StartNode n -> $"{nameof(StartNode)} {n}"
            | EndNode n -> $"{nameof(EndNode)} {n}"

        // Graph that contains all nodes, edges, and subgraphs
        let graph = DotGraph("DFG", true)

        // Create DOT start and end nodes for all types
        let startEndNodes =
            ocdfg.Nodes
            |> List.choose (fun n ->
                match n with
                | StartNode objType ->
                    let node = DotNode(StartNode objType |> nodeName)
                    node.Label <- objType
                    node.Shape <- DotNodeShapeAttribute DotNodeShape.Ellipse
                    Some node
                | EndNode objType ->
                    let node = DotNode(EndNode objType |> nodeName)
                    node.Label <- objType
                    node.Shape <- DotNodeShapeAttribute DotNodeShape.Underline
                    Some node
                | _ -> None)
        startEndNodes |> List.iter (fun n -> graph.Elements.Add n)

        // Create DOT edges for all edges in the graph
        let edges =
            ocdfg.Edges
            |> List.map (fun (a, b, e) ->
                let edge = DotEdge(nodeName a, nodeName b)
                edge.Label <- e.Statistics.Frequency.ToString()
                edge
            )
        edges |> List.iter (fun e -> graph.Elements.Add e)

        /// Add DOT nodes without any kind of grouping by namespace
        let addNodesWithoutNamespaces (graph: DotGraph) (ocdfg: DirectedGraph<Node, Edge>) : DotGraph =
            graph

        /// Add DOT nodes by grouping nodes into sub-graphs based on their namespace
        let addNodesWithNamespaces (graph: DotGraph) (ocdfg: DirectedGraph<Node, Edge>) (tree: ListTree<string * DotSubGraph>) : DotGraph =
            graph

        // Get list of unique fully-qualified namespaces
        let namespaces =
            ocdfg.Nodes
            |> List.choose (fun n -> match n with | EventNode n -> Some n | _ -> None)
            |> List.map (fun n -> n.Namespace)
            |> List.distinct

        // Determine whether there is any namespace information, generate the DOT graph, and finally compile it
        match namespaces with
        | [] | [""] -> addNodesWithoutNamespaces graph ocdfg
        | _ ->
            ([|'.'|], namespaces)
            ||> namespaceTree
            |> addSubGraphs
            |> addNodesWithNamespaces graph ocdfg
        |> fun g -> g.Compile(true)


    let oldcode (ocdfg: DirectedGraph<Node, Edge>) =

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
