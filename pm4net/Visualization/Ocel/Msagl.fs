namespace pm4net.Visualization.Ocel

open System
open System.IO
open pm4net.Types
open pm4net.Utilities
open Microsoft.Msagl.Core.Geometry
open Microsoft.Msagl.Core.Geometry.Curves
open Microsoft.Msagl.Drawing
open Microsoft.Msagl.Layout.Layered
open Microsoft.Msagl.Miscellaneous

type Msagl private () =

    /// Write a graph to a SVG string
    static member private graph2svg (graph: Graph) =
        let ms = new MemoryStream()
        let writer = new StreamWriter(ms)
        let svgWriter = SvgGraphWriter(writer.BaseStream, graph)
        svgWriter.Write()
        ms.Position <- 0
        let sr = new StreamReader(ms)
        sr.ReadToEnd()

    /// Create a node from an event node
    static member private createEventNode (eventNode: EventNode<NodeInfo>) =
        let node = Node(eventNode.Name)
        node.LabelText <- eventNode.Name
        node.Label.Width <- 100
        node.Label.Height <- 50
        node.Attr.Shape <- Shape.Box
        node.Attr.FillColor <-
            match eventNode.Info with
            | Some info ->
                match info.Level with
                | Some level ->
                    match level with
                    | Debug | Verbose -> Color.White
                    | Information -> Color.LightGray
                    | Warning -> Color.Orange
                    | Error -> Color.Red
                    | Fatal -> Color.DarkRed
                    | _ -> Color.White
                | _ -> Color.White
            | _ -> Color.White
        // TODO: font color
        node

    /// Convert an Object-Centric Directly-Follows-Graph (OC-DFG) into a graph with the help of MSAGL, rendered as a SVG
    static member OcDfg2Msagl (ocdfg: DirectedGraph<Node<NodeInfo>, Edge<EdgeInfo>>) (groupByNamespace: bool) =

        /// Convert a System.Drawing color to a MSAGL color
        let msColorToMsaglColor (color: Drawing.Color) =
            Color(color.A, color.R, color.G, color.B)

        /// https://stackoverflow.com/a/3722671/2102106
        let (|Prefix|_|) (p:string) (s:string) =
            if s.StartsWith(p) then
                Some(s.Substring(p.Length))
            else
                None

        // Graph that contains all nodes, edges, and subgraphs
        let graph = Graph()

        // Assign random colors to each object type to use them for edge colors
        let typeColors = Helpers.typeColors ocdfg.Nodes

        /// Find the maximum frequency of edges for all object types
        let typeMaxFrequencies = Helpers.typeMaxFrequencies ocdfg.Edges

        // Create start and end nodes for all types
        let startEndNodes =
            ocdfg.Nodes
            |> List.choose (fun n ->
                match n with
                | StartNode objType ->
                    let node = Helpers.nodeName n |> Node
                    node.LabelText <- objType
                    node.Label.Width <- 60 // TODO
                    node.Label.Height <- 40 // TODO
                    node.Attr.Shape <- Shape.Ellipse
                    node.Attr.FillColor <- typeColors[objType] |> msColorToMsaglColor
                    Some node
                | EndNode objType ->
                    let node = Helpers.nodeName n |> Node
                    node.LabelText <- objType
                    node.Label.Width <- 60 // TODO
                    node.Label.Height <- 40 // TODO
                    node.Attr.Shape <- Shape.Plaintext
                    //node.Attr.Color <- typeColors[objType] |> msColorToMsaglColor
                    node.Attr.FillColor <- typeColors[objType] |> msColorToMsaglColor
                    Some node
                | _ -> None)
        startEndNodes |> List.iter (fun n -> graph.AddNode(n))

        // Create edges
        ocdfg.Edges
        |> List.iter (fun (a, b, e) ->
            let edge = graph.AddEdge(Helpers.nodeName a, e.Weight.ToString(), Helpers.nodeName b)
            edge.Attr.Color <- (match e.Type with | Some objType -> typeColors[objType] |> msColorToMsaglColor | _ -> Color.Black)
            edge.Attr.LineWidth <- Helpers.scaleToRange 1f 5f 1f (typeMaxFrequencies[e.Type] |> float32) (e.Weight |> float32) |> float)

        // Add event nodes without any kind of grouping by namespace
        let addNodesWithoutNamespaces (graph: Graph) nodes =
            nodes
            |> List.map (fun n -> Msagl.createEventNode n)
            |> List.iter (fun n -> graph.AddNode n)

        // Get list of unique fully-qualified namespaces
        let namespaces = ocdfg.Nodes |> Helpers.namespaceList

        // Determine whether there is any namespace information, generate the DOT graph, and finally compile it
        let eventNodes = ocdfg.Nodes |> List.choose (fun n -> match n with | EventNode n -> Some n | _ -> None)
        let separators = [|'.'|]

        // Add event nodes
        match namespaces, groupByNamespace with
        | [], _ | [""], _ | _, false -> eventNodes |> addNodesWithoutNamespaces graph
        | _ ->
            ()
            //(separators, namespaces)
            //||> OcelHelpers.NamespaceTree
            // TODO

        // Create geometry
        graph.CreateGeometryGraph() |> ignore

        // Define geometrz for nodes
        graph.Nodes |> Seq.iter (fun n ->
            match n.Id with
            | Prefix (nameof(StartNode)) rest -> n.GeometryNode.BoundaryCurve <- CurveFactory.CreateEllipse(30, 20, Point(0, 0))
            | Prefix (nameof(EndNode)) rest -> n.GeometryNode.BoundaryCurve <- CurveFactory.CreateRectangle(120, 60, Point(0, 0))
            | _ -> n.GeometryNode.BoundaryCurve <- CurveFactory.CreateRectangleWithRoundedCorners(60, 40, 3, 2, Point(0, 0))
        )

        // Calculate layout and return as SVG
        LayoutHelpers.CalculateLayout(graph.GeometryGraph, SugiyamaLayoutSettings(), null)
        Msagl.graph2svg graph
