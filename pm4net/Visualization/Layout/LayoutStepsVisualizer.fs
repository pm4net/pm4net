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
