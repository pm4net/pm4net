namespace pm4net.Visualization.Layout

open System.Text
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

        let getColor = function
            | 0 -> "red"
            | _ -> "black"

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
