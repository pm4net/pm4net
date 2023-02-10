namespace pm4net.Visualization.Layout

open OCEL.Types
open pm4net.Types.Dfg
open pm4net.Utilities

/// Type to represent a sequence of nodes and edges, where an edge consists of two nodes
type private SequenceElement<'a> =
    | Node of 'a
    | Edge of 'a * 'a

type private Variation<'a> = {
    Events: 'a list
    Sequence: SequenceElement<'a> list
}

[<AbstractClass; Sealed>]
type StableGraphLayout private () =

    /// <summary>
    /// Compute a Global Ranking for all activities in an event log.
    /// Based on <see href="https://doi.org/10.1111/cgf.13723">Mennens, R.J.P., Scheepens, R. and Westenberg, M.A. (2019), A stable graph layout algorithm for processes. Computer Graphics Forum, 38: 725-737</see>
    /// and <see href="https://robinmennens.github.io/Portfolio/stablegraphlayouts.html">Graph layout stability in process mining</see>
    /// </summary>
    static member ComputeGlobalRanking (log: OcelLog) =

        /// Sort traces based on their importance
        let importanceSort (trace, freq) =
            freq

        /// Compute the sequence of a trace (interleaved nodes and edges) (Definition 4.1.2 of Mennes 2018)
        let sequence trace =
            trace
            |> List.pairwise
            |> List.mapi (fun i (a, b) ->
                match i with
                | 0 -> [Node(a); Edge(a, b); Node(b)]
                | _ -> [Edge(a, b); Node(b)]
            )
            |> List.concat

        /// Removes direct repetitions in a list (e.g. [A,A,B,C,C] -> [A,B,C]). Section 2.1 of Mennens 2018.
        let removeDirectRepetitions comparer trace =
            ([], trace) ||> List.fold (fun state nextValue ->
                match state |> List.tryLast with
                | None -> [nextValue]
                | Some last -> if comparer last nextValue then state else state @ [nextValue])

        let computeRanking (log: OcelLog) =
            // Discover the traces based on the referenced object type and discard the event ID's
            let traces =
                log
                |> OcelHelpers.OrderedTracesOfFlattenedLog
                |> Helpers.mapNestedList snd
                |> List.map (fun t -> t |> removeDirectRepetitions (fun a b -> a.Activity = b.Activity))

            let importanceSorted =
                traces
                // TODO: This removes all other event information, so if it is needed, it should be included here. Or otherwise use groupBy to keep all, and count differently.
                |> List.countBy (fun trace -> trace |> List.map (fun e -> e.Activity))
                |> List.sortByDescending importanceSort

            // Count all unique variations and how often they appear
            let sequences =
                importanceSorted
                |> List.map (fun (t, _) -> { Events = t; Sequence = t |> sequence })

            0

        log.ObjectTypes
        |> Seq.map (fun t -> t, OcelHelpers.Flatten log t)
        |> Map.ofSeq
        |> Map.map (fun _ flattenedLog -> computeRanking flattenedLog)

    /// <summary>
    /// Compute a stable graph layout for the discovered graph.
    /// Based on <see href="https://doi.org/10.1111/cgf.13723">Mennens, R.J.P., Scheepens, R. and Westenberg, M.A. (2019), A stable graph layout algorithm for processes. Computer Graphics Forum, 38: 725-737</see>
    /// </summary>
    static member Compute (graph: DirectedGraph<Node, Edge>) =
        0
