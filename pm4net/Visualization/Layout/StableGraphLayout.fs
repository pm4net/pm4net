namespace pm4net.Visualization.Layout

open OCEL.Types
open pm4net.Types.Dfg
open pm4net.Utilities

type GlobalRankGraph = DirectedGraph<string * int, int> // Nodes: activity and rank, Edges: frequency

/// Type to represent a sequence of nodes and edges, where an edge consists of two nodes
type private SequenceElement<'a> =
    | Node of 'a
    | Edge of 'a * 'a

type private Variation<'a, 'b> = {
    Events: 'a list
    Sequence: (SequenceElement<'a> * 'b) list
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
            let wSquaredSum = variation.Sequence |> List.sumBy (fun s -> match s with | Node _, _ -> 0 | Edge _, freq -> pown freq 2)
            let vLenSquared = pown variation.Events.Length 2
            wSquaredSum * vLenSquared

        /// Compute the sequence of a trace (interleaved nodes and edges) (Definition 4.1.2 of Mennes 2018)
        let simpleSequence freq trace =
            match trace with
            | [] -> []
            | [single] -> [Node(single), freq]
            | _ -> 
                trace
                |> List.pairwise
                |> List.mapi (fun i (a, b) ->
                    match i with
                    | 0 -> [Node(a), 0; Edge(a, b), freq; Node(b), 0]
                    | _ -> [Edge(a, b), freq; Node(b), 0])
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
            |> List.map (fun (t, cnt) -> { Events = t; Frequency = cnt; Sequence = (cnt, t) ||> simpleSequence })
            |> List.sortByDescending importanceSort // Importance-sort the variants so that the most important/frequent variants come first

        /// Extract all continuous sequences that only contain nodes and/or edges that are not yet in the rank graph (Mennens 2019, Section 3.1.1)
        /// Uses sequence to efficiently access last element when adding new items to the last sequence.
        let newSequence rankGraph variation =
            // For each sequence element, find out whether it already exists in the global rank graph.
            // If it does not, the element and subsequent elements form a new sequence that will be added to the graph.
            // If it does already exist, increment the frequency of the edge elements by the frequency of the new edge sequence.
            let partition =
                variation.Sequence
                |> Seq.indexed
                |> Seq.map (fun (idx, elem) ->
                    match elem with
                    | Node n, _ -> idx, rankGraph.Nodes |> List.exists (fun (act, _) -> act = n)
                    | Edge (a, b), _ -> idx, rankGraph.Edges |> List.exists (fun ((actA, _), (actB, _), _) -> actA = a && actB = b))

            // Build the initial state of the folder by adding the first sequence element when it is new, otherwise an empty list
            let initialState = if partition |> Seq.head |> snd |> not then variation.Sequence[partition |> Seq.head |> fst] |> Seq.singleton |> Seq.singleton else [[]]
            (initialState, partition |> Seq.pairwise) ||> Seq.fold (fun state (last, current) ->
                // Fold over the list of sequences, and determine whether a new sequence needs to be started based on the info about the last element
                match last, current with
                | (_, true), (_, false) ->
                    // Add a new sequence with the only element being the current element
                    variation.Sequence[fst current] |> Seq.singleton |> Seq.singleton |> Seq.append state
                | (_, false), (_, false) ->
                    // Append to the last sequence because the last element was new
                    let lastIdx = Seq.length state - 1
                    let updatedSeq = variation.Sequence[fst current] |> Seq.singleton |> Seq.append (state |> Seq.last)
                    state |> Seq.updateAt lastIdx updatedSeq
                | _ -> state
            ) |> Seq.filter (fun l -> l |> Seq.isEmpty |> not)

        /// Insert a new variation into an existing global rank graph
        let insertSequence rankGraph variation =

            /// Get the lowest rank encountered so far for any node
            let lowestRank rankGraph =
                if rankGraph.Nodes.IsEmpty then 0 else rankGraph.Nodes |> List.minBy snd |> snd

            /// Get the rank of a node with a given name
            let rankOfNode rankGraph node =
                rankGraph.Nodes |> List.find (fun n -> fst n = node) |> snd

            /// Insert a Type 1 or 5 sequence into a global rank graph 
            let insertNodeToNode rankGraph seq =
                let (nodes, edges) = seq |> List.partition (fun s -> match s with | Node _, _ -> true | Edge _, _ -> false)
                // First insert all nodes in the sequence to ensure that they exist before adding edges, which need to know the rank of the connecting nodes
                // There may be the same node multiple times in the same sequence (not directly after each other). In that case, the later nodes are discarded here (TODO: check what paper is doing)
                let rg =
                    ((rankGraph, rankGraph |> lowestRank), nodes |> List.distinctBy fst) ||> List.fold (fun (graph, nextRank) (node, _) ->
                        { graph with
                            Nodes =
                                match node with
                                | Node n -> (n, nextRank) :: graph.Nodes
                                | _ -> graph.Nodes
                        },
                        nextRank + 1
                    ) |> fst
                // Now add the edges in the sequence
                (rg, edges) ||> List.fold (fun graph (edge, freq) ->
                    { graph with
                        Edges =
                            match edge with
                            | Edge (a, b) -> ((a, a |> rankOfNode graph), (b, b |> rankOfNode graph), freq) :: graph.Edges
                            | _ -> graph.Edges
                    }
                )

            let newSeqs = newSequence rankGraph variation |> Seq.map List.ofSeq |> List.ofSeq // Get the new sequences that have not been added to the global rank graph yet
            // TODO: Also find the sequence elements that do already exist, in order to increase their frequency with the current variation's frequency
            (rankGraph, newSeqs) ||> Seq.fold (fun graph newSeq ->
                match newSeq with
                | [Node _, _] -> insertNodeToNode rankGraph newSeq // Type 1
                | [Edge _, _] -> graph // Type 2
                | _ ->
                    match newSeq, newSeq |> List.rev |> List.head with
                    | (Node _, _) :: _, (Edge _, _) -> graph // Type 3
                    | (Edge _, _) :: _, (Node _, _) -> graph // Type 4
                    | (Node _, _) :: _, (Node _, _) -> insertNodeToNode rankGraph newSeq // Type 5
                    | (Edge _, _) :: _, (Edge _, _) -> graph // Type 6
                    | _ -> graph
            )

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
