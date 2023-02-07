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
                match children |> List.tryFind (fun (Node((value, _), _)) -> value = head) with
                | Some next -> Graphviz.findNodeWithPath next tail
                | None -> None

    /// Add all subgraphs from a tree to a given DOT graph
    static member private addSubgraphsToGraph (tree: ListTree<string * DotSubGraph>) (graph: IDotGraph) : unit =
        match tree with
        | Node((_, subGraph), children) ->
            graph.Elements.Add subGraph
            children |> List.iter (fun c -> Graphviz.addSubgraphsToGraph c subGraph)

    /// Create a DOT node from en Event node
    static member private createEventNode eventNode =
        let node = DotNode($"{eventNode.Name}")
        node.SetCustomAttribute("label", $"<<B>{eventNode.Name}</B><BR/>{eventNode.Statistics.Frequency}>") |> ignore
        node.Shape <- DotNodeShapeAttribute DotNodeShape.Rectangle
        node.Style <- DotNodeStyleAttribute DotNodeStyle.Filled
        node.FillColor <-
            match eventNode.Level with
            | Debug | Verbose -> DotFillColorAttribute System.Drawing.Color.White
            | Information -> DotFillColorAttribute System.Drawing.Color.LightGray
            | Warning -> DotFillColorAttribute System.Drawing.Color.Orange
            | Error -> DotFillColorAttribute System.Drawing.Color.Red
            | Fatal -> DotFillColorAttribute System.Drawing.Color.DarkRed
            | _ -> DotFillColorAttribute System.Drawing.Color.White
        node.FontColor <-
            match eventNode.Level with
            | Debug | Verbose -> DotFontColorAttribute System.Drawing.Color.Black
            | Information -> DotFontColorAttribute System.Drawing.Color.Black
            | Warning -> DotFontColorAttribute System.Drawing.Color.Black
            | Error -> DotFontColorAttribute System.Drawing.Color.White
            | Fatal -> DotFontColorAttribute System.Drawing.Color.White
            | _ -> DotFontColorAttribute System.Drawing.Color.Black
        node

    /// Convert an Object-Centric Directly-Follows-Graph (OC-DFG) into a DOT graph
    static member OcDfg2Dot (ocdfg: DirectedGraph<Node, Edge>) (groupByNamespace: bool) =

        /// Get a unique name for a node
        let nodeName = function
            | EventNode n -> n.Name
            | StartNode n -> $"{nameof(StartNode)} {n}"
            | EndNode n -> $"{nameof(EndNode)} {n}"

        // Graph that contains all nodes, edges, and subgraphs
        let graph = DotGraph("DFG", true)

        // Assign random colors to each object type to use them for edge colors
        let typeColors =
            ocdfg.Nodes
            |> List.choose (fun n -> match n with | StartNode n -> Some n | _ -> None)
            |> List.map (fun obj -> obj, Helpers.randomColor())
            |> Map.ofList

        // Create DOT start and end nodes for all types
        let startEndNodes =
            ocdfg.Nodes
            |> List.choose (fun n ->
                match n with
                | StartNode objType ->
                    let node = DotNode(StartNode objType |> nodeName)
                    node.Label <- objType
                    node.Shape <- DotNodeShapeAttribute DotNodeShape.Ellipse
                    node.Style <- DotNodeStyleAttribute DotNodeStyle.Filled
                    node.FillColor <- DotFillColorAttribute typeColors[objType]
                    Some node
                | EndNode objType ->
                    let node = DotNode(EndNode objType |> nodeName)
                    node.Label <- objType
                    node.Shape <- DotNodeShapeAttribute DotNodeShape.Underline
                    node.Style <- DotNodeStyleAttribute DotNodeStyle.Filled
                    node.FillColor <- DotFillColorAttribute typeColors[objType]
                    Some node
                | _ -> None)
        startEndNodes |> List.iter (fun n -> graph.Elements.Add n)

        // Create DOT edges for all edges in the graph
        let edges =
            ocdfg.Edges
            |> List.map (fun (a, b, e) ->
                let edge = DotEdge(nodeName a, nodeName b)
                edge.Label <- e.Statistics.Frequency.ToString()
                edge.FontColor <- DotFontColorAttribute typeColors[e.Type]
                edge.Color <- DotColorAttribute typeColors[e.Type]
                edge
            )
        edges |> List.iter (fun e -> graph.Elements.Add e)

        /// Add DOT nodes without any kind of grouping by namespace
        let addNodesWithoutNamespaces (graph: DotGraph) nodes =
            nodes
            |> List.map (fun n -> Graphviz.createEventNode n)
            |> List.iter (fun n -> graph.Elements.Add n)
            graph

        /// Add DOT nodes by grouping nodes into sub-graphs based on their namespace
        let addNodesWithNamespaces separators (graph: DotGraph) (nodes: EventNode list) (tree: ListTree<string * DotSubGraph>) : DotGraph =
            nodes
            |> List.iter (fun n ->
                let subGraph = n.Namespace.Split(separators) |> List.ofArray |> Graphviz.findNodeWithPath tree
                match subGraph with
                | Some subGraph -> Graphviz.createEventNode n |> subGraph.Elements.Add
                | None -> ()
            )

            graph |> Graphviz.addSubgraphsToGraph tree
            graph

        // Get list of unique fully-qualified namespaces
        let namespaces =
            ocdfg.Nodes
            |> List.choose (fun n -> match n with | EventNode n -> Some n | _ -> None)
            |> List.map (fun n -> n.Namespace)
            |> List.distinct

        // Determine whether there is any namespace information, generate the DOT graph, and finally compile it
        let eventNodes = ocdfg.Nodes |> List.choose (fun n -> match n with | EventNode n -> Some n | _ -> None)
        let separators = [|'.'|]

        match namespaces, groupByNamespace with
        | [], _ | [""], _ | _, false ->
            eventNodes |> addNodesWithoutNamespaces graph
        | _ ->
            (separators, namespaces)
            ||> OcelHelpers.NamespaceTree
            |> Graphviz.addSubGraphs
            |> addNodesWithNamespaces separators graph eventNodes
        |> fun g -> g.Compile(true)
