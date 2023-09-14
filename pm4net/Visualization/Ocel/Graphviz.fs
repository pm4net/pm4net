namespace pm4net.Visualization.Ocel

open System
open pm4net.Types
open pm4net.Types.Trees
open DotNetGraph
open DotNetGraph.Node
open DotNetGraph.Edge
open DotNetGraph.SubGraph
open DotNetGraph.Attributes
open DotNetGraph.Extensions
open pm4net.Utilities

[<AbstractClass; Sealed>]
type Graphviz private () =

    // Add empty sub-graphs with the corresponding ID's to each node in a tree
    static member private addSubGraphs (tree: ListTree<string>) =
        match tree with
        | Node(node, children) ->
            // Unique ID to avoid duplicate ID's when single parts of namespaces exist multiple times in different paths
            let id = Guid.NewGuid().ToString("N")
            let subGraph = DotSubGraph($"cluster_{id}")
            subGraph.Label <- node
            Node((node, subGraph), children |> List.map Graphviz.addSubGraphs)

    /// Find a node in a tree given a specific path, ignoring the value of the root node
    static member private findNodeWithPath tree path =
        match tree with
        | Node(_, children) ->
            match path with
            | [] -> match tree with | Node((_, value), _) -> Some value
            | head :: tail ->
                match children |> Seq.tryFind (fun (Node((value, _), _)) -> value = head) with
                | Some next -> Graphviz.findNodeWithPath next tail
                | None -> None

    /// Add all subgraphs from a tree to a given DOT graph
    static member private addSubgraphsToGraph (tree: ListTree<string * DotSubGraph>) (graph: IDotGraph) : unit =
        match tree with
        | Node((_, subGraph), children) ->
            graph.Elements.Add subGraph
            children |> Seq.iter (fun c -> Graphviz.addSubgraphsToGraph c subGraph)

    /// Create a DOT node from en Event node
    static member private createEventNode (eventNode: EventNode<NodeInfo>) =
        let node = DotNode($"{eventNode.Name}")
        node.SetCustomAttribute("label", $"<<B>{eventNode.Name}</B><BR/>{match eventNode.Info with | Some info -> info.Frequency | _ -> 0}>") |> ignore
        node.Shape <- DotNodeShapeAttribute DotNodeShape.Rectangle
        node.Style <- DotNodeStyleAttribute DotNodeStyle.Filled
        node.FillColor <-
            match eventNode.Info with
            | Some info ->
                match info.Level with
                | Some level ->
                    match level with
                    | Debug | Verbose -> DotFillColorAttribute System.Drawing.Color.White
                    | Information -> DotFillColorAttribute System.Drawing.Color.LightGray
                    | Warning -> DotFillColorAttribute System.Drawing.Color.Orange
                    | Error -> DotFillColorAttribute System.Drawing.Color.Red
                    | Fatal -> DotFillColorAttribute System.Drawing.Color.DarkRed
                    | _ -> DotFillColorAttribute System.Drawing.Color.White
                | _ -> DotFillColorAttribute System.Drawing.Color.White
            | _ -> DotFillColorAttribute System.Drawing.Color.White
        node.FontColor <-
            match eventNode.Info with
            | Some info ->
                match info.Level with
                | Some level ->
                    match level with
                    | Debug | Verbose -> DotFontColorAttribute System.Drawing.Color.Black
                    | Information -> DotFontColorAttribute System.Drawing.Color.Black
                    | Warning -> DotFontColorAttribute System.Drawing.Color.Black
                    | Error -> DotFontColorAttribute System.Drawing.Color.White
                    | Fatal -> DotFontColorAttribute System.Drawing.Color.White
                    | _ -> DotFontColorAttribute System.Drawing.Color.Black
                | _ -> DotFontColorAttribute System.Drawing.Color.Black
            | _ -> DotFontColorAttribute System.Drawing.Color.Black
        node

    /// Convert an Object-Centric Directly-Follows-Graph (OC-DFG) into a DOT graph
    static member OcDfg2Dot (ocdfg: DirectedGraph<Node<NodeInfo>, Edge<EdgeInfo>>) (groupByNamespace: bool) =

        // Graph that contains all nodes, edges, and subgraphs
        let graph = DotGraph("DFG", true)

        // Assign random colors to each object type to use them for edge colors
        let typeColors = Helpers.typeColors ocdfg.Nodes

        /// Find the maximum frequency of edges for all object types
        let typeMaxFrequencies = Helpers.typeMaxFrequencies ocdfg.Edges

        // Create DOT start and end nodes for all types
        let startEndNodes =
            ocdfg.Nodes
            |> Seq.choose (fun n ->
                match n with
                | StartNode objType ->
                    let node = DotNode(StartNode objType |> Helpers.nodeName)
                    node.Label <- objType
                    node.Shape <- DotNodeShapeAttribute DotNodeShape.Ellipse
                    node.Style <- DotNodeStyleAttribute DotNodeStyle.Filled
                    node.FillColor <- DotFillColorAttribute typeColors[objType]
                    Some node
                | EndNode objType ->
                    let node = DotNode(EndNode objType |> Helpers.nodeName)
                    node.Label <- objType
                    node.Shape <- DotNodeShapeAttribute DotNodeShape.Underline
                    node.Color <- DotColorAttribute typeColors[objType]
                    node.FontColor <- DotFontColorAttribute typeColors[objType]
                    Some node
                | _ -> None)
        startEndNodes |> Seq.iter (fun n -> graph.Elements.Add n)

        // Create DOT edges for all edges in the graph
        let edges =
            ocdfg.Edges
            |> Seq.map (fun (a, b, e) ->
                let edge = DotEdge(Helpers.nodeName a, Helpers.nodeName b)
                edge.Label <- e.Weight.ToString()
                edge.FontColor <- DotFontColorAttribute (match e.Type with | Some objType -> typeColors[objType] | _ -> Drawing.Color.Black)
                edge.Color <- DotColorAttribute (match e.Type with | Some objType -> typeColors[objType] | _ -> Drawing.Color.Black)
                edge.PenWidth <- DotPenWidthAttribute (Helpers.scaleToRange 1f 5f 1f (typeMaxFrequencies[e.Type] |> float32) (e.Weight |> float32))
                edge.SetCustomAttribute("weight", $"{e.Weight}") |> ignore
                edge
            )
        edges |> Seq.iter (fun e -> graph.Elements.Add e)

        /// Add DOT nodes without any kind of grouping by namespace
        let addNodesWithoutNamespaces (graph: DotGraph) nodes =
            nodes
            |> Seq.map (fun n -> Graphviz.createEventNode n)
            |> Seq.iter (fun n -> graph.Elements.Add n)
            graph

        /// Add DOT nodes by grouping nodes into sub-graphs based on their namespace
        let addNodesWithNamespaces separators (graph: DotGraph) (nodes: EventNode<NodeInfo> seq) (tree: ListTree<string * DotSubGraph>) : DotGraph =
            nodes
            |> Seq.iter (fun n ->
                match n.Info with
                | Some info ->
                    match info.Namespace with
                    | Some ns ->
                        let subGraph = ns.Split(separators) |> List.ofArray |> Graphviz.findNodeWithPath tree
                        match subGraph with
                        | Some subGraph -> Graphviz.createEventNode n |> subGraph.Elements.Add
                        | _ -> ()
                    | _ -> ()
                | _ -> ()
            )

            graph |> Graphviz.addSubgraphsToGraph tree
            graph

        // Get list of unique fully-qualified namespaces
        let namespaces = ocdfg.Nodes |> Helpers.namespaceList

        // Determine whether there is any namespace information, generate the DOT graph, and finally compile it
        let eventNodes = ocdfg.Nodes |> Seq.choose (fun n -> match n with | EventNode n -> Some n | _ -> None)
        let separators = [|'.'|]

        match namespaces |> Seq.toList, groupByNamespace with
        | [], _ | [""], _ | _, false ->
            eventNodes |> addNodesWithoutNamespaces graph
        | _ ->
            (separators, namespaces)
            ||> OcelHelpers.NamespaceTree
            |> Graphviz.addSubGraphs
            |> addNodesWithNamespaces separators graph eventNodes
        |> fun g -> g.Compile(true)
