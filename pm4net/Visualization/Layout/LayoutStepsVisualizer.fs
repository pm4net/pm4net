namespace pm4net.Visualization.Layout

open System.Text
open System.Globalization
open pm4net.Types.GraphLayout

[<AbstractClass; Sealed>]
type LayoutStepsVisualizer private () =

    static member globalRankGraphToDot (graph: GlobalRankGraph) =

        let getNodesByRanks (nodes: (string * int) list) =
            nodes |> List.groupBy snd |> List.sortBy fst

        let getLevelsString (nodes: (string * int) list) =
            let ranks = nodes |> getNodesByRanks |> List.map (fun (r, _) -> r.ToString())
            String.concat " -> " ranks

        let addNodes (nodes: (string * int) list) (sb: StringBuilder) =
            let ranks = nodes |> getNodesByRanks
            (sb, ranks) ||> List.fold (fun sb (rank, nodes) ->
                let nodesStr = ("", nodes) ||> List.fold (fun state node -> state + $" \"{fst node}\"")
                sb.AppendLine $"{{rank=same; {rank}{nodesStr}}}")

        let addEdges (edges: ((string * int) * (string * int) * int) list) (sb: StringBuilder) =
            (sb, edges) ||> List.fold (fun sb ((a, _), (b, _), freq) -> sb.AppendLine $"\"{a}\" -> \"{b}\" [label=\"{freq}\", weight={freq}]")

        let sb = StringBuilder()
        let sb = sb.AppendLine "digraph dfg {"
        let sb = sb.AppendLine "node[fontsize=24, shape=plaintext]"
        let sb = graph.Nodes |> getLevelsString |> sb.AppendLine
        let sb = sb.AppendLine ""
        let sb = sb.AppendLine "node[fontsize=20, shape=box]"
        let sb = sb |> addNodes graph.Nodes
        let sb = sb.AppendLine ""
        let sb = sb |> addEdges graph.Edges
        let sb = sb.AppendLine "}"
        sb.ToString()

    /// Only works with Neato or Fdp layout engine
    static member nodeSequenceGraphToDot (nsg: NodeSequenceGraph) =

        let getDiscoveryIndex = function
            | Real(_, idx, _)
            | Virtual(_, idx) -> idx

        let getRank = function
            | Real(r, _, _)
            | Virtual(r, _) -> r

        let getShape = function
            | Real _ -> "square"
            | Virtual _ -> "circle"

        let getId = function
            | Real(_, _, n) -> n
            | Virtual(r, dIdx) -> $"{r}_{dIdx}"

        let getNodesByRanks (nodes: SequenceNode list) =
            nodes |> List.groupBy (fun n -> getRank n)

        let addNodes (nodes: SequenceNode list) (sb: StringBuilder) =
            let ranks = getNodesByRanks nodes |> Map.ofList
            (sb, nodes) ||> List.fold (fun sb node ->
                let rank = getRank node
                let nodesByDiscIdx = ranks[rank] |> List.sortBy getDiscoveryIndex
                let posInSorted = nodesByDiscIdx |> List.findIndex (fun n -> n = node)
                let isBackbone = posInSorted = 0
                sb.AppendLine $""""{getId node}" [label="{getDiscoveryIndex node}" pos="{posInSorted},-{rank}!" shape={getShape node} color={if isBackbone then "red" else "black"} style=filled fillcolor="#fff2cc"]""")

        let addEdges (edges: (SequenceNode * SequenceNode) list) (sb: StringBuilder) =
            (sb, edges) ||> List.fold (fun sb (a, b) ->
                sb.AppendLine $"\"{getId a}\" -> \"{getId b}\" [arrowhead=none]")

        let sb = StringBuilder()
        let sb = sb.AppendLine "digraph dfg {"
        let sb = sb.AppendLine "splines=false"
        let sb = sb.AppendLine ""
        let sb = sb |> addNodes nsg.Nodes
        let sb = sb.AppendLine ""
        let sb = sb |> addEdges nsg.Edges
        let sb = sb.AppendLine "}"
        sb.ToString()

    /// Only works with Neato or Fdp layout engine
    static member nodeSequenceGraphToDot (nsg: GlobalOrderNodeSequenceGraph) =

        let getDiscoveryIndex = function
            | Real(_, idx, _)
            | Virtual(_, idx) -> idx

        let getRank = function
            | Real(r, _, _)
            | Virtual(r, _) -> r

        let getShape = function
            | Real _ -> "square"
            | Virtual _ -> "circle"

        let getId = function
            | Real(_, _, n) -> n
            | Virtual(r, dIdx) -> $"{r}_{dIdx}"

        let getNodesByRanks (nodes: (int * SequenceNode) list) =
            nodes |> List.groupBy (fun (_, n) -> getRank n)

        let addNodes (nodes: (int * SequenceNode) list) (sb: StringBuilder) =
            let ranks = getNodesByRanks nodes |> Map.ofList
            (sb, nodes) ||> List.fold (fun sb (x, node) ->
                let rank = getRank node
                let isBackbone = ranks[rank] |> List.minBy (fun (_, n) -> getDiscoveryIndex n) |> snd = node
                sb.AppendLine $""""{getId node}" [label="{getDiscoveryIndex node}" pos="{x},-{rank}!" shape={getShape node} color={if isBackbone then "red" else "black"} style=filled fillcolor="#fff2cc"]""")

        let addEdges (edges: ((int * SequenceNode) * (int * SequenceNode)) list) (sb: StringBuilder) =
            (sb, edges) ||> List.fold (fun sb ((_, a), (_, b)) ->
                sb.AppendLine $"\"{getId a}\" -> \"{getId b}\" [arrowhead=none]")

        let sb = StringBuilder()
        let sb = sb.AppendLine "digraph dfg {"
        let sb = sb.AppendLine "splines=false"
        let sb = sb.AppendLine ""
        let sb = sb |> addNodes nsg.Nodes
        let sb = sb.AppendLine ""
        let sb = sb |> addEdges nsg.Edges
        let sb = sb.AppendLine "}"
        sb.ToString()

    /// Only works with Neato layout engine
    static member discoveredGraphToDot (graph: DiscoveredGraph) =

        let getFloat (num: float32) =
            num.ToString("0.00", CultureInfo.InvariantCulture)

        let getEdgeColor = function
            | ConstrainedReal _, ConstrainedReal _
            | ConstrainedReal _, ConstrainedVirtual _
            | ConstrainedVirtual _ , ConstrainedReal _
            | ConstrainedVirtual _, ConstrainedVirtual _ -> "red"
            | _, UnconstrainedVirtual _
            | UnconstrainedVirtual _, _ -> "green"

        let getId = function
            | ConstrainedReal(_, _, name) -> name
            | ConstrainedVirtual(pos, _) -> $"v {pos.X},{pos.Y}"
            | UnconstrainedVirtual(pos, conn) -> $"uv {pos.X},{pos.Y} {conn.A}-{conn.B}"

        let addNodes (nodes: GraphNode list) (sb: StringBuilder) =
            (sb, nodes) ||> List.fold (fun sb node ->
                match node with
                | ConstrainedReal(pos, _, name) -> sb.AppendLine $""""{getId node}" [label="" xlabel=<<font color="black" point-size="6">{getFloat pos.X},{pos.Y}</font>> pos="{getFloat pos.X},-{pos.Y}!" shape=square style=filled fillcolor="#fff2cc" width="0.1"]"""
                | ConstrainedVirtual(pos, idx) -> sb.AppendLine $""""{getId node}" [label="" xlabel=<<font color="red" point-size="6">{getFloat pos.X}</font>> pos="{getFloat pos.X},-{pos.Y}!" shape=circle style=filled fillcolor="red" width="0.1"]"""
                | UnconstrainedVirtual(pos, conn) -> sb.AppendLine $""""{getId node}" [label="" xlabel=<<font color="green" point-size="6">{getFloat pos.X}</font>> pos="{getFloat pos.X},-{pos.Y}!" shape=circle style=filled fillcolor=green width="0.1"]""")

        let addEdges (edges: (GraphNode * GraphNode) list) (sb: StringBuilder) =
            (sb, edges) ||> List.fold (fun sb (a, b) ->
                sb.AppendLine $""""{getId a}" -> "{getId b}" [label="" arrowsize="0.5" color="{getEdgeColor(a, b)}"]""")

        let sb = StringBuilder()
        let sb = sb.AppendLine "digraph dfg {"
        let sb = sb.AppendLine "splines=true"
        let sb = sb.AppendLine ""
        let sb = sb |> addNodes graph.Nodes
        let sb = sb.AppendLine ""
        let sb = sb |> addEdges graph.Edges
        let sb = sb.AppendLine "}"
        sb.ToString()

    /// Only works with Neato or Fdp layout engine
    static member crossMinGraphToDot (crossMinNsg: CrossMinNsgWithPos) =

        let getAttrString (x: float32) rank color posColor =
            let xStr = x.ToString("0.00", CultureInfo.InvariantCulture)
            $"""[label="" xlabel=<<font color="{posColor}" point-size="6">{xStr}</font>> pos="{xStr},-{rank}!" style=filled fillcolor={color} shape=square width=0.1 color={color}]"""

        let getId = function
            | Sequence(_, n) ->
                match n with
                | Real(_, _, name) -> name
                | Virtual(r, idx) -> $"{r}_{idx}"
            | NonSequence(r, a, b) -> $"{r}_{a}_{b}"

        let addNodes (nodes: (float32 * CrossMinNode) list) (sb: StringBuilder) =
            (sb, nodes) ||> List.fold (fun sb (x, n) ->
                let id = getId n
                match n with
                | Sequence(_, node) ->
                    match node with
                    | Real(rank, _, name) -> sb.AppendLine $""""{id}" {getAttrString x rank "black" "red"}"""
                    | Virtual(rank, dIdx) -> sb.AppendLine $""""{id}" {getAttrString x rank "red" "red"}"""
                | NonSequence(rank, a, b) -> sb.AppendLine $""""{id}" {getAttrString x rank "green" "green"}""")

        let addEdges (edges: ((float32 * CrossMinNode) * (float32 * CrossMinNode) * bool) list) (sb: StringBuilder) =
            (sb, edges) ||> List.fold (fun sb ((_, a), (_, b), _) ->
                sb.AppendLine $""""{getId a}" -> "{getId b}" """)

        let sb = StringBuilder()
        let sb = sb.AppendLine "digraph dfg {"
        let sb = sb |> addNodes crossMinNsg.Nodes
        let sb = sb.AppendLine ""
        let sb = sb |> addEdges crossMinNsg.Edges
        let sb = sb.AppendLine "}"
        sb.ToString()
