namespace pm4net.Visualization.Layout

open OCEL.Types
open pm4net.Types.Dfg
open pm4net.Utilities

type GlobalRankGraph = DirectedGraph<string * int, int> // Nodes: activity and rank, Edges: frequency

/// Type to represent a sequence of nodes and edges, where an edge consists of two nodes
type private SequenceElement<'a> =
    | Node of 'a
    | Edge of 'a * 'a

type private Variation<'a> = {
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
            let wSquaredSum = variation.Sequence |> List.sumBy (fun s -> match s with | Node _ -> 0 | Edge _ -> pown variation.Frequency 2)
            let vLenSquared = pown variation.Events.Length 2
            wSquaredSum * vLenSquared

        /// Compute the sequence of a trace (interleaved nodes and edges) (Definition 4.1.2 of Mennes 2018)
        let simpleSequence trace =
            match trace with
            | [] -> []
            | [single] -> [Node(single)]
            | _ -> 
                trace
                |> List.pairwise
                |> List.mapi (fun i (a, b) ->
                    match i with
                    | 0 -> [Node(a); Edge(a, b); Node(b)]
                    | _ -> [Edge(a, b); Node(b)])
                |> List.concat

        /// Removes direct repetitions in a list (e.g. [A,A,B,C,C] -> [A,B,C]). Section 2.1 of Mennens 2018.
        let removeDirectRepetitions comparer trace =
            ([], trace) ||> List.fold (fun state nextValue ->
                match state |> List.tryLast with
                | None -> [nextValue]
                | Some last -> if comparer last nextValue then state else state @ [nextValue])

        /// Extract the unique variations in the flattened log and order them by importance
        let variationsInLog log =
            log
            |> OcelHelpers.OrderedTracesOfFlattenedLog // Get traces based on referenced object
            |> Helpers.mapNestedList snd // Discard the event ID as it is not relevant
            |> List.map (fun t -> t |> removeDirectRepetitions (fun a b -> a.Activity = b.Activity)) // Remove direct repetitions of events with the same activity
            |> List.countBy (fun t -> t |> List.map (fun e -> e.Activity)) // Extract only activity name and count the occurrences of each variant/path
            |> List.map (fun (t, cnt) -> { Events = t; Frequency = cnt; Sequence = t |> simpleSequence })
            |> List.sortByDescending importanceSort // Importance-sort the variants so that the most important/frequent variants come first

        /// Extract all continuous sequences that only contain nodes and/or edges that are not yet in the rank graph (Mennens 2019, Section 3.1.1)
        let newSequence (rg: GlobalRankGraph) (var: Variation<string>) =
            // For each sequence element, find out whether it already exists in the global rank graph.
            // If it does not, the element and subsequent elements form a new sequence that will be added to the graph.
            // If it does already exist, increment the frequency of the edge elements by the frequency of the new edge sequence.
            let partition =
                var.Sequence
                |> List.indexed
                |> List.map (fun (idx, elem) ->
                    match elem with
                    | Node n -> idx, rg.Nodes |> List.exists (fun (act, _) -> act = n)
                    | Edge (a, b) -> idx, rg.Edges |> List.exists (fun ((actA, _), (actB, _), _) -> actA = a && actB = b))

            // Build the initial state of the folder by adding the first sequence element when it is new, otherwise an empty list
            let initialState = if snd partition.Head |> not then [[var.Sequence[fst partition.Head]]] else [[]]
            (initialState, partition |> List.tail |> List.pairwise) ||> List.fold (fun state (last, current) ->
                // Fold over the list, and determine whether a new sequence needs to be started based on the info about the last element
                // The first element in the partition is skipped as it was already looked at when building the initial state
                match last with
                | (idx, true) -> state @ List.singleton [var.Sequence[idx]] // Start a new sequence because the last element was not new
                | (idx, false) -> // Append to the last sequence because the last element was new
                    let rev = state |> List.rev
                    let rev = rev |> List.updateAt 0 (rev.Head @ List.singleton var.Sequence[idx])
                    rev |> List.rev
            ) |> List.filter (fun l -> not l.IsEmpty)

        /// Insert a new variation into an existing global rank graph
        let insertSequence (rg: GlobalRankGraph) (var: Variation<string>) : GlobalRankGraph =
            let newSeq = newSequence rg var
            rg

        /// Compute a ranking for a flattened event log
        let computeRanking log =
            ({ Nodes = []; Edges = [] }, log |> variationsInLog) ||> List.fold insertSequence

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
