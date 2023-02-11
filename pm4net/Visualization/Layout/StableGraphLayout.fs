namespace pm4net.Visualization.Layout

open OCEL.Types
open pm4net.Types.Dfg
open pm4net.Utilities

/// Type to represent a sequence of nodes and edges, where an edge consists of two nodes and a weight
type (*private*) SequenceElement<'a> =
    | Node of 'a
    | Edge of ('a * 'a) * int

type (*private*) Variation<'a> = {
    Events: 'a list
    Sequence: SequenceElement<'a> list
    Frequency: int
}

[<AbstractClass; Sealed>]
type StableGraphLayout private () =

    /// <summary>
    /// Compute a Global Ranking for all activities in an event log.
    /// Expects the log to not contain identical objects with different ID's. Use <see cref="OCEL.Types.OcelLog.MergeDuplicateObjects"/> to merge them beforehand.
    /// Based on <see href="https://doi.org/10.1111/cgf.13723">Mennens, R.J.P., Scheepens, R. and Westenberg, M.A. (2019), A stable graph layout algorithm for processes. Computer Graphics Forum, 38: 725-737</see>
    /// and <see href="https://robinmennens.github.io/Portfolio/stablegraphlayouts.html">Graph layout stability in process mining</see>
    /// </summary>
    static member ComputeGlobalRanking (log: OcelLog) =

        /// Sort traces based on their importance (sum of w^2 * |v|^2)
        let importanceSort variation =
            let wSquaredSum = variation.Sequence |> List.sumBy (fun s -> match s with | Node _ -> 0 | Edge (_, w) -> pown w 2)
            let vLenSquared = pown variation.Events.Length 2
            wSquaredSum * vLenSquared

        /// Compute the sequence of a trace (interleaved nodes and edges) (Definition 4.1.2 of Mennes 2018)
        let simpleSequence cnt trace =
            trace
            |> List.pairwise
            |> List.mapi (fun i (a, b) ->
                match i with
                | 0 -> [Node(a); Edge((a, b), cnt); Node(b)]
                | _ -> [Edge((a, b), cnt); Node(b)]
            )
            |> List.concat

        /// Removes direct repetitions in a list (e.g. [A,A,B,C,C] -> [A,B,C]). Section 2.1 of Mennens 2018.
        let removeDirectRepetitions comparer trace =
            ([], trace) ||> List.fold (fun state nextValue ->
                match state |> List.tryLast with
                | None -> [nextValue]
                | Some last -> if comparer last nextValue then state else state @ [nextValue])

        /// Compute a ranking for a flattened event log
        let computeRanking log =
            log
            |> OcelHelpers.OrderedTracesOfFlattenedLog // Get traces based on referenced object
            |> Helpers.mapNestedList snd // Discard the event ID as it is not relevant
            |> List.map (fun t -> t |> removeDirectRepetitions (fun a b -> a.Activity = b.Activity)) // Remove direct repetitions of events with the same activity
            |> List.countBy (fun t -> t |> List.map (fun e -> e.Activity)) // Extract only activity name and count the occurrences of each variant/path
            |> List.map (fun (t, cnt) -> { Events = t; Frequency = cnt; Sequence = (cnt, t) ||> simpleSequence })
            |> List.sortByDescending importanceSort // Importance-sort the variants so that the most important/frequent variants come first

        // Compute global ranking from each perspective when flattening logs for different object types
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
