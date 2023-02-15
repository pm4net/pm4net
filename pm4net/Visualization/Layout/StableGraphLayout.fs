namespace pm4net.Visualization.Layout

open OCEL.Types
open pm4net.Types.Dfg
open pm4net.Utilities

/// A directed graph that represents the global rank graph. Nodes consist of activity name and rank, and edges store their frequency.
type GlobalRankGraph = DirectedGraph<string * int, int>

/// Type to represent a sequence of nodes and edges, where an edge consists of two nodes
type private SequenceElement<'a> =
    | Node of 'a
    | Edge of 'a * 'a

/// Type to represent a variation of a trace, with all its events, a sequence of nodes and edges, and the frequency of the variation
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

        /// Compute the sequence of a trace, not accounting for the existing global rank graph (interleaved nodes and edges) (Definition 4.1.2 of Mennes 2018)
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
                |> List.indexed
                |> List.map (fun (idx, elem) ->
                    match elem with
                    | Node n, _ -> idx, rankGraph.Nodes |> List.exists (fun (act, _) -> act = n)
                    | Edge (a, b), _ -> idx, rankGraph.Edges |> List.exists (fun ((actA, _), (actB, _), _) -> actA = a && actB = b))

            // Build the initial state of the folder by adding the first sequence element when it is new, otherwise an empty list
            let initialState = if partition |> List.head |> snd |> not then variation.Sequence[partition |> List.head |> fst] |> List.singleton |> List.singleton else [[]]
            (initialState, partition |> List.pairwise) ||> List.fold (fun state (last, current) ->
                // Fold over the list of sequences, and determine whether a new sequence needs to be started based on the info about the last element
                match last, current with
                | (_, true), (_, false) ->
                    // Add a new sequence with the only element being the current element
                    variation.Sequence[fst current] |> List.singleton |> List.singleton |> List.append state
                | (_, false), (_, false) ->
                    // Append to the last sequence because the last element was new
                    let lastIdx = List.length state - 1
                    let updatedSeq = variation.Sequence[fst current] |> List.singleton |> List.append (state |> List.last)
                    state |> List.updateAt lastIdx updatedSeq
                | _ -> state
            ) |> List.filter (fun l -> l |> List.isEmpty |> not)

        /// Insert a new variation into an existing global rank graph
        let insertSequence (rankGraph, components) variation =

            /// Get the lowest rank encountered so far for any node
            let lowestRank rankGraph =
                if rankGraph.Nodes.IsEmpty then 0 else rankGraph.Nodes |> List.minBy snd |> snd

            /// Get the rank of a node with a given name
            let rankOfNode rankGraph node =
                rankGraph.Nodes |> List.find (fun n -> fst n = node) |> snd

            /// Find the index of the set that contains a given value in a list of sets, if it exists
            let findContainingSetIndex components value =
                List.tryFindIndex (fun set -> set |> Set.contains value) components

            /// Traverse the rank graph downwards from a starting point to find the reachable nodes within a given number of ranks (Algorithm 4 from Mennens 2018)
            let rec traverse rankGraph numRanks node visited =

                /// Get the edges that match the traversal criteria, using a boolean flag to get either the origin or destination of the node
                let getEdges out =
                    rankGraph.Edges |> List.filter (fun e ->
                        let ((a, _), (b, _), _) = e
                        let n, v = if out then a, b else b, a
                        if n = node && visited |> List.contains v |> not then
                            let nRank, vRank = n |> rankOfNode rankGraph, v |> rankOfNode rankGraph
                            vRank > nRank && vRank - nRank <= numRanks
                        else false)

                let visited = node :: visited
                let outEdges = getEdges true
                let visited = (visited, outEdges) ||> List.fold (fun vis (_, (target, _), _) -> (target, vis) ||> traverse rankGraph numRanks)
                let inEdges = getEdges false
                let visited = (visited, inEdges) ||> List.fold (fun vis ((target, _), _, _) -> (target, vis) ||> traverse rankGraph numRanks)
                visited

            /// Add a new node to the rank graph. Expects new nodes to be disjoint from all other nodes, so always add nodes before edges!
            let addNode rankGraph components node =
                { rankGraph with Nodes = node :: rankGraph.Nodes }, ([(fst node |> Set.singleton)], components) ||> List.append

            /// Update a node's rank in a rank graph, changing the edges that reference the nodes with it
            let updateNode (rankGraph: GlobalRankGraph) node newRank =
                let nodeIdx = rankGraph.Nodes |> List.findIndex (fun n -> fst n = node)
                let outEdges = rankGraph.Edges |> List.indexed |> List.filter (fun (_, ((a, _), _, _)) -> a = node) |> List.map (fun (i, ((a, _), b, freq)) -> i, ((a, newRank), b, freq))
                let inEdges = rankGraph.Edges |> List.indexed |> List.filter (fun (_, (_, (b, _), _)) -> b = node) |> List.map (fun (i, (a, (b, _), freq)) -> i, (a, (b, newRank), freq))
                { rankGraph with
                    Nodes = rankGraph.Nodes |> List.updateAt nodeIdx (node, newRank)
                    Edges = (rankGraph.Edges, outEdges @ inEdges) ||> List.fold (fun s (i, e) -> s |> List.updateAt i e)
                }

            /// Add a new edge to the rank graph, updating the the connected components
            let addEdge rankGraph components edge =
                // If the edge connects two disjoint sets, merge them together
                let ((a, _), (b, _), _) = edge
                let components =
                    match a |> findContainingSetIndex components, b |> findContainingSetIndex components with
                    | Some aIdx, Some bIdx ->
                        if aIdx <> bIdx then
                            let aSet, bSet = components[aIdx], components[bIdx]
                            let components = if aIdx > bIdx then components |> List.removeAt aIdx |> List.removeAt bIdx else components |> List.removeAt bIdx |> List.removeAt aIdx
                            components |> List.append [aSet |> Set.union bSet]
                        else components
                    | _ -> failwith $"Could not find component that contains either {a} or {b}. Always add all nodes before adding edges between them."

                // Add the edge to the rank graph and return it together with the updated component list
                { rankGraph with Edges = edge :: rankGraph.Edges }, components

            /// Insert a Type 1 or 5 sequence into a global rank graph 
            let insertNodeToNode rankGraph components seq =
                let (nodes, edges) = seq |> List.partition (fun s -> match s with | Node _, _ -> true | Edge _, _ -> false)
                // First insert all nodes in the sequence to ensure that they exist before adding edges, which need to know the rank of the connecting nodes
                // There may be the same node multiple times in the same sequence (not directly after each other). In that case, the later nodes are discarded here (behaviour not specified in Mannens 2018)
                let rankGraph, components, _ =
                    ((rankGraph, components, rankGraph |> lowestRank), nodes |> List.distinctBy fst) ||> List.fold (fun (graph, components, nextRank) (node, _) ->
                        match node with
                        | Node n ->
                            let graph, components = (n, nextRank) |> addNode graph components
                            graph, components, nextRank + 1
                        | Edge _ -> graph, components, nextRank
                    )
                // Now add the edges in the sequence
                ((rankGraph, components), edges) ||> List.fold (fun (graph, components) (edge, freq) ->
                    match edge with
                    | Edge (a, b) -> ((a, a |> rankOfNode graph), (b, b |> rankOfNode graph), freq) |> addEdge graph components
                    | _ -> graph, components
                )

            /// Insert a Type 2 sequence (single edge) into a global rank graph
            let insertSingleEdge rankGraph components (a, b, freq) =
                let rankA = rankOfNode rankGraph a
                let rankB = rankOfNode rankGraph b
                match rankB - rankA with
                | diff when diff > 0 -> // Forward edge
                    ((a, rankA), (b, rankB), freq) |> addEdge rankGraph components
                | diff when diff < 0 -> // Backward edge
                    match a |> findContainingSetIndex components, b |> findContainingSetIndex components with
                    | Some aIdx, Some bIdx when aIdx = bIdx ->
                        // Part of the same component, so allow backwards edge
                        ((a, rankA), (b, rankB), freq) |> addEdge rankGraph components
                    | Some _, Some bIdx ->
                        // Edge connects two previously unconnected components, so shift component B down to create forward edge (Algorithm 2 from Mennens 2018)
                        let dist = rankA - rankB + 1
                        let rankGraph = (rankGraph, components[bIdx]) ||> Set.fold (fun graph node ->
                            updateNode graph node ((rankOfNode graph node) + dist))
                        ((a, rankA), (b, rankB), freq) |> addEdge rankGraph components
                    | _ -> rankGraph, components
                | 0 -> // Horizontal edge, shift nodes downwards (Algorithm 3 from Mennens 2018)
                    let numRanks = 1
                    let visited = traverse rankGraph numRanks b [a]
                    let rankGraph, components = ((a, rankA), (b, rankB), freq) |> addEdge rankGraph components
                    (rankGraph, rankGraph.Nodes) ||> List.fold (fun graph (node, _) ->
                        // Shift node's rank down by numRanks, and update newly added edge to reflect new rank
                        if node <> a && visited |> List.contains node then
                            (node, rankB + numRanks) ||> updateNode rankGraph
                        else graph
                    ), components
                | _ -> rankGraph, components // Impossible, but to silence about incomplete pattern matching due to when expressions

            /// Insert a Type 3 or 4 sequence into a global rank graph, expecting a function to modify the rank counter for each node encountered
            let insertAsymmetricSequence rankModifier rankGraph components seq start =
                let startRank = rankOfNode rankGraph start
                // First insert all nodes by going through the sequence, adjusting the rank with each node encountered
                let rankGraph, components, _ =
                    ((rankGraph, components, startRank |> rankModifier), seq) ||> List.fold (fun (graph, components, nextRank) elem ->
                        match elem with
                        | Node n, _ ->
                            let graph, components = (n, nextRank) |> addNode graph components
                            graph, components, nextRank |> rankModifier
                        | _ -> graph, components, nextRank
                    )
                // Now add the edges in the sequence
                ((rankGraph, components), seq) ||> List.fold (fun (graph, components) elem ->
                    match elem with
                    | Edge (a, b), freq -> ((a, rankOfNode graph a), (b, rankOfNode graph b), freq) |> addEdge graph components
                    | _ -> graph, components
                )

            /// Insert a Type 6 sequence into a global rank graph (see Alogrithm 5 in Mennens 2018)
            let insertEdgeToEdge rankGraph components seq =
                let (u, x) = match seq |> List.head with | Edge (a, b), _ -> a, b | _ -> System.ArgumentException("Starting element must be an edge", nameof seq) |> raise
                let (y, v) = match seq |> List.last with | Edge (a, b), _ -> a, b | _ -> System.ArgumentException("Last element must be an edge", nameof seq) |> raise
                System.NotImplementedException("TODO: Type 6, edge to edge") |> raise

            // Get the new sequences that have not been added to the global rank graph yet
            let newSeqs = newSequence rankGraph variation |> Seq.map List.ofSeq |> List.ofSeq

            // Get the edges that are already in the graph, so that the frequencies can be incremented (returns only index of edge and frequency to add)
            let alreadyKnownEdges = variation.Sequence |> List.choose (fun se ->
                match se with
                | Edge(a, b), freq ->
                    match rankGraph.Edges |> List.tryFindIndex (fun ((edgeA, _), (edgeB, _), _) -> a = edgeA && b = edgeB) with
                    | Some idx -> Some (idx, freq)
                    | _ -> None
                | _ -> None)

            // Update the global rank graph with the frequencies of edges that are not new but appear in variation
            let rankGraph = (rankGraph, alreadyKnownEdges) ||> List.fold (fun graph (idx, freqToIncrement) ->
                let (a, b, existingFreq) = graph.Edges[idx]
                { graph with Edges = graph.Edges |> List.updateAt idx (a, b, existingFreq + freqToIncrement) }
            )

            // Insert the new sequence elements into the global rank graph according to the techniques for the different types of sequences in Mennens 2018
            ((rankGraph, components), newSeqs) ||> List.fold (fun (graph, components) newSeq ->
                match newSeq with
                | [Node _, _] -> insertNodeToNode rankGraph components newSeq // Type 1
                | [Edge (a, b), freq] -> insertSingleEdge rankGraph components (a, b, freq) // Type 2
                | _ ->
                    match newSeq, newSeq |> List.rev |> List.head with
                    | (Node _, _) :: _, (Edge (_ , destination), _) -> insertAsymmetricSequence (fun r -> r - 1) rankGraph components (newSeq |> List.rev) destination // Type 3
                    | (Edge (origin, _), _) :: _, (Node _, _) -> insertAsymmetricSequence (fun r -> r + 1) rankGraph components newSeq origin // Type 4
                    | (Node _, _) :: _, (Node _, _) -> insertNodeToNode rankGraph components newSeq // Type 5
                    | (Edge _, _) :: _, (Edge _, _) -> insertEdgeToEdge rankGraph components newSeq // Type 6
                    | _ -> graph, components)

        /// Compute a ranking for a flattened event log
        let computeRanking log =
            (({ Nodes = []; Edges = [] }, List.empty), log |> variationsInLog) ||> List.fold insertSequence |> fst

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
