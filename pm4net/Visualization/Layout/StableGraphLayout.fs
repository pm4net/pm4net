namespace pm4net.Visualization.Layout

open OCEL.Types
open pm4net.Types
open pm4net.Types.Dfg
open pm4net.Utilities

/// A directed graph that represents the global rank graph. Nodes consist of activity name and rank, and edges store their frequency.
type GlobalRankGraph = DirectedGraph<string * int, int>

/// Type to represent a sequence of nodes and edges, where an edge consists of two nodes
type SequenceElement<'a> =
    | Node of 'a
    | Edge of 'a * 'a

/// Type to represent a variation of a trace, with all its events, a sequence of nodes and edges, and the frequency of the variation
type private Variation<'a, 'b> = {
    Events: 'a list
    Sequence: (SequenceElement<'a> * 'b) list
    Frequency: int
}

/// Types of nodes that can be found in a node sequence graph
type SequenceNode =
    | Real of Rank: int * DiscoveryIndex: int * Name: string
    | Virtual of Rank: int * DiscoveryIndex: int

/// An undirected graph that represents the node sequence graph of a given rank graph and skeleton (data structure is technically directed, but use edges as two-way connections)
type NodeSequenceGraph = DirectedGraph<SequenceNode>

[<AbstractClass; Sealed>]
type StableGraphLayout private () =

    /// Sort traces based on their importance (sum of w^2 * |v|^2)
    static member private importanceSort variation =
        let wSquaredSum = variation.Sequence |> List.sumBy (fun s -> match s with | Node _, _ -> 0 | Edge _, freq -> pown freq 2)
        let vLenSquared = pown variation.Events.Length 2
        wSquaredSum * vLenSquared

    /// Compute the sequence of a trace, not accounting for the existing global rank graph (interleaved nodes and edges) (Definition 4.1.2 of Mennes 2018)
    static member private simpleSequence freq trace =
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
    static member private removeDirectRepetitions comparer trace =
        ([], trace) ||> List.fold (fun state nextValue ->
            match state |> List.tryLast with
            | None -> [nextValue]
            | Some last -> if comparer last nextValue then state else state @ [nextValue])

    /// Extract the unique variations in the flattened log and order them by importance
    static member private variationsInLog objType log =
        log
        |> OcelHelpers.OrderedTracesOfFlattenedLog // Get traces based on referenced object
        |> Helpers.mapNestedList snd // Discard the event ID as it is not relevant
        |> List.map (fun events -> events |> StableGraphLayout.removeDirectRepetitions (fun a b -> a.Activity = b.Activity)) // Remove direct repetitions of events with the same activity
        |> List.map (fun events ->
            // Add a start and end node for the object type to each trace, if it is specified
            match objType with
            | Some objType ->
                let startEvent = { Activity = $"{Constants.objectTypeStartNode} {objType}"; Timestamp = (events |> List.head |> fun e -> e.Timestamp.AddTicks(-1)); OMap = []; VMap = Map.empty }
                let endEvent = { Activity = $"{Constants.objectTypeEndNode} {objType}"; Timestamp = (events |> List.last |> fun e -> e.Timestamp.AddTicks(1)); OMap = []; VMap = Map.empty }
                startEvent :: events @ [endEvent]
            | None -> events)
        |> List.countBy (fun t -> t |> List.map (fun e -> e.Activity)) // Extract only activity name and count the occurrences of each variant/path
        |> List.map (fun (t, cnt) -> { Events = t; Frequency = cnt; Sequence = (cnt, t) ||> StableGraphLayout.simpleSequence })

    /// Extract all continuous sequences that only contain nodes and/or edges that are not yet in the rank graph (Mennens 2019, Section 3.1.1)
    /// Uses sequence to efficiently access last element when adding new items to the last sequence.
    static member private newSequences rankGraph variation =
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

    /// Get the rank of a node with a given name
    static member private rankOfNode rankGraph node =
        rankGraph.Nodes |> List.find (fun n -> fst n = node) |> snd

    /// Compute a global ranking for a list of variations, according to the Algorithm from Mennens 2018 & 2019
    static member private computeGlobalRankingForVariations variations =

        /// Update a node's rank in a rank graph, changing the edges that reference the nodes with it
        let updateNode rankGraph node newRank =
            let nodeIdx = rankGraph.Nodes |> List.findIndex (fun n -> fst n = node)
            let outEdges = rankGraph.Edges |> List.indexed |> List.filter (fun (_, ((a, _), _, _)) -> a = node) |> List.map (fun (i, ((a, _), b, freq)) -> i, ((a, newRank), b, freq))
            let inEdges = rankGraph.Edges |> List.indexed |> List.filter (fun (_, (_, (b, _), _)) -> b = node) |> List.map (fun (i, (a, (b, _), freq)) -> i, (a, (b, newRank), freq))
            { rankGraph with
                Nodes = rankGraph.Nodes |> List.updateAt nodeIdx (node, newRank)
                Edges = (rankGraph.Edges, outEdges @ inEdges) ||> List.fold (fun s (i, e) -> s |> List.updateAt i e)
            }

        /// Normalize the ranks of a rank graph so that the first node is always on rank 0 (can become negative during processing)
        let normalizeRanks rankGraph =
            let lowestRank = rankGraph.Nodes |> List.minBy snd |> snd
            if lowestRank <> 0 then
                (rankGraph, rankGraph.Nodes) ||> List.fold (fun graph (node, rank) -> updateNode graph node (rank - lowestRank))
            else rankGraph

        /// Reverse the order of nodes and edges in the graph, as they are added to the beginning of the list when discovering (purely for intuitiveness)
        let reverseNodeAndEdgeOrder rankGraph : DirectedGraph<_,_> =
            { rankGraph with Nodes = rankGraph.Nodes |> List.rev; Edges = rankGraph.Edges |> List.rev }

        /// Update the edges that are already in the graph, but occur in the new variation, by incrementing their frequencies with that of the variation
        let updateAlreadyKnownEdges rankGraph variation =
            let alreadyKnownEdges = variation.Sequence |> List.choose (fun se ->
                match se with
                | Edge(a, b), freq ->
                    match rankGraph.Edges |> List.tryFindIndex (fun ((edgeA, _), (edgeB, _), _) -> a = edgeA && b = edgeB) with
                    | Some idx -> Some (idx, freq)
                    | _ -> None
                | _ -> None)

            (rankGraph, alreadyKnownEdges) ||> List.fold (fun graph (idx, freqToIncrement) ->
                let (a, b, existingFreq) = graph.Edges[idx]
                { graph with Edges = graph.Edges |> List.updateAt idx (a, b, existingFreq + freqToIncrement) })

        /// Insert a sequence of edges and nodes that have not been seen before in the graph
        let insertSequence rankGraph components sequence =

            /// Get the lowest rank encountered so far for any node
            let lowestRank rankGraph =
                if rankGraph.Nodes.IsEmpty then 0 else rankGraph.Nodes |> List.minBy snd |> snd

            /// Find the index of the set that contains a given value in a list of sets, if it exists
            let findContainingComponentIndex components value =
                List.findIndex (fun set -> set |> Set.contains value) components

            /// Traverse the rank graph downwards from a starting point to find the reachable nodes within a given number of ranks (Algorithm 4 from Mennens 2018)
            let rec traverse rankGraph numRanks node visited =

                /// Get the edges that match the traversal criteria, using a boolean flag to get either the origin or destination of the node
                let getEdges out =
                    rankGraph.Edges |> List.filter (fun e ->
                        let ((a, _), (b, _), _) = e
                        let n, v = if out then a, b else b, a
                        if n = node && visited |> List.contains v |> not then
                            let nRank, vRank = n |> StableGraphLayout.rankOfNode rankGraph, v |> StableGraphLayout.rankOfNode rankGraph
                            vRank > nRank && vRank - nRank <= numRanks
                        else false)

                let visited = node :: visited
                let outEdges = getEdges true
                let visited = (visited, outEdges) ||> List.fold (fun vis (_, (target, _), _) -> (target, vis) ||> traverse rankGraph numRanks)
                let inEdges = getEdges false
                let visited = (visited, inEdges) ||> List.fold (fun vis ((target, _), _, _) -> (target, vis) ||> traverse rankGraph numRanks)
                visited |> List.distinct

            /// Add a new node to the rank graph. Expects new nodes to be disjoint from all other nodes, so always add nodes before edges!
            let addNode rankGraph components node : DirectedGraph<_,_> * _ =
                { rankGraph with Nodes = node :: rankGraph.Nodes }, ([(fst node |> Set.singleton)], components) ||> List.append

            /// Shift the destination of the edge and all nodes reachable via downward traversal by the a number of ranks (Algorithm 3 from Mennens 2018)
            let shiftNodes rankGraph a b numRanks =
                let visited = traverse rankGraph numRanks b [a]
                (rankGraph, rankGraph.Nodes) ||> List.fold (fun graph (node, _) ->
                    // Shift node's rank down by numRanks, and update newly added edge to reflect new rank
                    if node <> a && visited |> List.contains node then
                        (node, (StableGraphLayout.rankOfNode graph node) + numRanks) ||> updateNode graph
                    else graph
                )

            /// Merge two previously unconnected components together (Algorithm 2 from Mennens 2018)
            let mergeComponents rankGraph (components: Set<string> list) a b offset =
                let rankA = StableGraphLayout.rankOfNode rankGraph a
                let rankB = StableGraphLayout.rankOfNode rankGraph b
                let dist = rankA - rankB + offset
                let rankGraph = (rankGraph, components[b |> findContainingComponentIndex components]) ||> Set.fold (fun graph node ->
                    updateNode graph node ((StableGraphLayout.rankOfNode graph node) + dist))
                rankGraph, components

            /// Add a new edge to the rank graph, updating the the connected components. Excepts nodes of wedges to already exist, so always add nodes before edges!
            let addEdge rankGraph components edge : DirectedGraph<_,_> * _ =
                // If the edge connects two disjoint sets, merge them together
                let ((a, _), (b, _), _) = edge
                let components =
                    let aIdx, bIdx = a |> findContainingComponentIndex components, b |> findContainingComponentIndex components
                    if aIdx <> bIdx then
                        let aSet, bSet = components[aIdx], components[bIdx]
                        let components = if aIdx > bIdx then components |> List.removeAt aIdx |> List.removeAt bIdx else components |> List.removeAt bIdx |> List.removeAt aIdx
                        components |> List.append [aSet |> Set.union bSet]
                    else components

                // Add the edge to the rank graph and return it together with the updated component list
                { rankGraph with Edges = edge :: rankGraph.Edges }, components

            /// Insert a sequence of nodes and edges to the rank graph, updating the components along the way
            let insertSequenceIntoGraph rankModifier rankGraph components startRank seq =
                let (nodes, edges) = seq |> List.partition (fun s -> match s with | Node _, _ -> true | Edge _, _ -> false)

                // First insert all nodes in the sequence to ensure that they exist before adding edges, which need to know the rank of the connecting nodes
                // There may be the same node or same edge multiple times in the same sequence, however not directly after each other (filtered out previously).
                // This case is unspecified by Mannens 2018, but is handled in the following way here:
                // For multiple nodes, latter nodes are simply discarded.
                // For edges, latter edges edges are discarded, but the frequencies are summed up beforehand and added to the first occurrence.
                let rankGraph, components, _ =
                    ((rankGraph, components, startRank), nodes |> List.distinctBy fst) ||> List.fold (fun (graph, components, nextRank) node ->
                        match node with
                        | Node n, _ ->
                            let graph, components = (n, nextRank) |> addNode graph components
                            graph, components, nextRank |> rankModifier
                        | _ -> graph, components, nextRank)

                // Group edges of the same origin+destination together, and reduce them to single edges with their frequencies summed up
                let edges =
                    edges
                    |> List.groupBy (fun (e, _) ->
                        match e with
                        | Edge (a, b) -> a, b
                        | _ -> failwith "Nodes are not allowed here.")
                    |> List.map (fun (_, edges) -> edges |> List.reduce (fun (e, f1) (_, f2) -> e, f1 + f2))

                // Add the edges to the rank graph
                ((rankGraph, components), edges) ||> List.fold (fun (graph, components) (edge, freq) ->
                    match edge with
                    | Edge (a, b) -> ((a, a |> StableGraphLayout.rankOfNode graph), (b, b |> StableGraphLayout.rankOfNode graph), freq) |> addEdge graph components
                    | _ -> graph, components)

            /// Insert a Type 2 sequence (single edge) into a global rank graph
            let insertSingleEdge rankGraph components (a, b, freq) =
                let rankA = StableGraphLayout.rankOfNode rankGraph a
                let rankB = StableGraphLayout.rankOfNode rankGraph b
                match rankB - rankA with
                | 0 -> // Horizontal edge, shift nodes downwards (Algorithm 3 from Mennens 2018)
                    let rankGraph, components = ((a, rankA), (b, rankB), freq) |> addEdge rankGraph components
                    shiftNodes rankGraph a b 1, components
                | diff when diff > 0 -> // Forward edge, can simply be added
                    ((a, rankA), (b, rankB), freq) |> addEdge rankGraph components
                | diff when diff < 0 -> // Backward edge
                    match a |> findContainingComponentIndex components, b |> findContainingComponentIndex components with
                    | aIdx, bIdx when aIdx = bIdx ->
                        // Part of the same component, so allow backwards edge
                        ((a, rankA), (b, rankB), freq) |> addEdge rankGraph components
                    | _ ->
                        // Edge connects two previously unconnected components, so shift component B down to create forward edge (Algorithm 2 from Mennens 2018)
                        let rankGraph, components = mergeComponents rankGraph components a b 1
                        ((a, rankA), (b, rankB), freq) |> addEdge rankGraph components
                | _ -> rankGraph, components // Impossible, but to silence about incomplete pattern matching due to when expressions

            /// Insert a Type 6 sequence into a global rank graph (see Algorithm 5 in Mennens 2018)
            let insertEdgeToEdge rankGraph components seq =
                let (u, _) = match seq |> List.head with | Edge (a, b), _ -> a, b | _ -> System.ArgumentException("Starting element must be an edge", nameof seq) |> raise
                let (y, v) = match seq |> List.last with | Edge (a, b), _ -> a, b | _ -> System.ArgumentException("Last element must be an edge", nameof seq) |> raise
                let compU, compV = findContainingComponentIndex components u, findContainingComponentIndex components v
                let rankU, rankV = StableGraphLayout.rankOfNode rankGraph u, StableGraphLayout.rankOfNode rankGraph v
                let noOfNodes = seq |> List.choose (fun s -> match s with | Node n, _ -> Some n | _ -> None) |> List.distinct |> List.length

                // Decide what to do based on how the sequence fits into the existing graph
                if compU <> compV then
                    let rankGraph, components = mergeComponents rankGraph components u v (noOfNodes + 1)
                    insertSequenceIntoGraph (fun r -> r + 1) rankGraph components (rankU + 1) seq
                else
                    if rankU <= rankV then
                        let rankGraph, components = insertSequenceIntoGraph (fun r -> r + 1) rankGraph components (rankU + 1) seq
                        let freeRanks = rankV - rankU - 1
                        if noOfNodes > freeRanks then
                            shiftNodes rankGraph y v (StableGraphLayout.rankOfNode rankGraph y - rankV + 1), components
                        else rankGraph, components
                    else
                        let rankGraph, components = insertSequenceIntoGraph (fun r -> r - 1) rankGraph components (rankU - 1) seq
                        let freeRanks = rankU - rankV - 1
                        if noOfNodes > freeRanks then
                            shiftNodes rankGraph v y (rankV - StableGraphLayout.rankOfNode rankGraph y + 1), components
                        else rankGraph, components

            match sequence with
            | [Node _, _] -> insertSequenceIntoGraph (fun r -> r + 1) rankGraph components (rankGraph |> lowestRank) sequence // Type 1
            | [Edge (a, b), freq] -> insertSingleEdge rankGraph components (a, b, freq) // Type 2
            | _ ->
                match sequence, sequence |> List.rev |> List.head with
                | (Node _, _) :: _, (Edge (_ , dest), _) -> insertSequenceIntoGraph (fun r -> r - 1) rankGraph components ((dest |> StableGraphLayout.rankOfNode rankGraph) - 1) (sequence |> List.rev) // Type 3
                | (Edge (orig, _), _) :: _, (Node _, _) -> insertSequenceIntoGraph (fun r -> r + 1) rankGraph components ((orig |> StableGraphLayout.rankOfNode rankGraph) + 1) sequence // Type 4
                | (Node _, _) :: _, (Node _, _) -> insertSequenceIntoGraph (fun r -> r + 1) rankGraph components (rankGraph |> lowestRank) sequence // Type 5
                | (Edge _, _) :: _, (Edge _, _) -> insertEdgeToEdge rankGraph components sequence // Type 6
                | _ -> rankGraph, components

        /// Process a variation until no more new sequences can be added to the rank graph
        let rec processVariation variation skeleton (rankGraph, components) =
            match (rankGraph, variation) ||> StableGraphLayout.newSequences with
            | [] -> rankGraph, components, skeleton
            | seq :: _ ->
                // Insert the first found new sequence into the graph
                let rankGraph, components = seq |> insertSequence rankGraph components
                // Add the sequence to the skeleton if it contains at least one node (Definition 4.2.1 of Mennens 2018)
                let skeleton = if seq |> List.exists (fun elem -> match elem with | Node _, _ -> true | _ -> false) then skeleton @ [seq] else skeleton
                // Process the variation again until there are no more new sequences available in the variation
                (rankGraph, components) |> processVariation variation skeleton

        // Discover the final rank graph and the skeleton thereof, when adding all variations to an empty rank graph
        let rankGraph, _, skeleton =
            (({ Nodes = []; Edges = [] }, [], []), variations)
            ||> List.fold (fun (graph, components, skeleton) variation ->
                let graph = updateAlreadyKnownEdges graph variation
                (graph, components) |> processVariation variation skeleton)

        // Normalize the rank graph and order nodes and edges correctly, and return it together with the skeleton
        rankGraph |> normalizeRanks |> reverseNodeAndEdgeOrder, skeleton

    /// Compute a global order given a global rank graph and the process skeleton
    static member ComputeNodeSequenceGraph (rg: GlobalRankGraph) (skeleton: (SequenceElement<string> * int) list list) : NodeSequenceGraph =

        /// Try to find an edge between two nodes, disregarding the direction of the edge
        let tryFindEdge (nsg: NodeSequenceGraph) nodeA nodeB =
            nsg.Edges |> List.tryFind (fun (a, b) -> a = nodeA && b = nodeB || a = nodeB && b = nodeA)

        /// Find all edges that have the given node as the origin or destination, and return them with their index in the list
        let findEdgesWithNode (nsg: NodeSequenceGraph) node =
            nsg.Edges |> List.indexed |> List.filter (fun (_, (a, b)) -> a = node || b = node)

        /// Add an edge between two nodes if it doesn't already exist, disregarding edge direction
        let addEdgeIfNotExists (nsg: NodeSequenceGraph) nodeA nodeB =
            let edge = (nodeA, nodeB)
            match tryFindEdge nsg nodeA nodeB with
            | Some edge -> nsg, edge
            | None -> { nsg with Edges = edge :: nsg.Edges }, edge

        /// Try to find a real or virtual node by its rank and discovery index in a node sequence graph (NSG)
        let tryFindNode (nsg: NodeSequenceGraph) rank discoveryIndex =
            nsg.Nodes |> List.tryFindIndex (fun n ->
                match n with
                | Real(r, idx, _) -> r = rank && idx = discoveryIndex
                | Virtual(r, idx) -> r = rank && idx = discoveryIndex
            )

        /// Try to find a real node on a rank with a given name, disregarding the discovery index
        let tryFindRealNodeOnRank (nsg: NodeSequenceGraph) rank name =
            nsg.Nodes |> List.tryFindIndex (fun n ->
                match n with
                | Real(r, _, name) -> r = rank && name = name
                | _ -> false)

        /// Add a virtual node to the NSG, if there isn't already an existing node at this rank with the same discovery index
        let addVirtualNodeIfNotExists (nsg: NodeSequenceGraph) rank discoveryIndex =
            let virtualNode = Virtual(rank, discoveryIndex)
            match tryFindNode nsg rank discoveryIndex with
            | Some nodeIndex -> nsg, nsg.Nodes[nodeIndex]
            | None -> { nsg with Nodes = virtualNode :: nsg.Nodes }, virtualNode

        /// Add a real node to the NSG, replacing any virtual nodes that may already be there. Also updates any edges that reference the replaced node.
        let addOrReplaceRealNode (nsg: NodeSequenceGraph) rank discoveryIndex name =
            let newNode = Real(rank, discoveryIndex, name)
            match tryFindRealNodeOnRank nsg rank name with
            | Some foundNodeIdx ->
                match nsg.Nodes[foundNodeIdx] with
                | Real _ -> nsg, nsg.Nodes[foundNodeIdx]
                | _ -> failwith "Not possible to find virtual node when searching for real ones."
            | None ->
                match tryFindNode nsg rank discoveryIndex with
                | Some foundNodeIdx ->
                    match nsg.Nodes[foundNodeIdx] with
                    | Real _ -> nsg, nsg.Nodes[foundNodeIdx] // Don't modify the graph, return the found node
                    | Virtual _ ->
                        // Replace the virtual node with a real one, and update edges referencing it
                        let updatedEdges = Virtual(rank, discoveryIndex) |> findEdgesWithNode nsg |> List.map (fun (i, (a, b)) -> i, if nsg.Nodes[foundNodeIdx] = a then (newNode, b) else (a, newNode))
                        { nsg with
                            Nodes = nsg.Nodes |> List.updateAt foundNodeIdx newNode
                            Edges = (nsg.Edges, updatedEdges) ||> List.fold (fun edges (i, e) -> edges |> List.updateAt i e)
                        }, newNode
                | None -> { nsg with Nodes = newNode :: nsg.Nodes }, newNode

        /// Add virtual nodes between two nodes on different ranks where required and other nodes don't already exist
        let rec addVirtualNodesAndEdgesBetweenRanks (nsg: NodeSequenceGraph) rankA nodeA rankB discoveryIndex =
            let upwards = rankA - rankB >= 0 // Whether the direction is upwards or downwards
            let nextRank = rankA + if upwards then -1 else 1 // The next rank to consider in the given direction
            let nsg, nextNode = addVirtualNodeIfNotExists nsg nextRank discoveryIndex // Add a virtual node at the next rank, if necessary
            let nsg, _ = addEdgeIfNotExists nsg nodeA nextNode // Add an edge between the current and the next rank, if necessary

            // When the next rank is already the desired destination, we only have to add an edge between the current rank and the destination
            match nextRank = rankB with
            | false -> addVirtualNodesAndEdgesBetweenRanks nsg nextRank nextNode rankB discoveryIndex // Recursively add virtual nodes and edges until done
            | _ -> nsg // All done!

        (({ Nodes = []; Edges = [] }: DirectedGraph<_>), skeleton |> List.indexed) ||> List.fold (fun nsg (idx, seq) ->
            let edges = seq |> List.filter (fun elem -> match elem with | Edge _, _ -> true | _ -> false)
            (nsg, edges) ||> List.fold (fun nsg edge ->
                match edge with
                | Edge (a, b), _ ->
                    // Find the ranks of A and B in the global rank graph
                    let rankA, rankB = StableGraphLayout.rankOfNode rg a, StableGraphLayout.rankOfNode rg b

                    // Add the real nodes of the edge origin and destination if they don't already exist, or replace
                    let nsg, nodeA = addOrReplaceRealNode nsg rankA idx a
                    let nsg, nodeB = addOrReplaceRealNode nsg rankB idx b

                    // If the nodes are on neighboring ranks, we can simply add the edge (if it doesn't already exist). Otherwise, we have to add virtual nodes in between.
                    match abs(rankA - rankB) with
                    | 1 -> addEdgeIfNotExists nsg nodeA nodeB |> fst
                    | _ -> addVirtualNodesAndEdgesBetweenRanks nsg rankA nodeA rankB idx
                | _ -> nsg
            )
        )

    /// <summary>
    /// Compute a Global Ranking for all activities in an event log, merging the flattened logs of all object types together to get a total view.
    /// Expects the log to not contain identical objects with different ID's. Use <see cref="OCEL.Types.OcelLog.MergeDuplicateObjects"/> to merge them beforehand.
    /// Based on <see href="https://doi.org/10.1111/cgf.13723">Mennens, R.J.P., Scheepens, R. and Westenberg, M.A. (2019), A stable graph layout algorithm for processes. Computer Graphics Forum, 38: 725-737</see>
    /// and <see href="https://robinmennens.github.io/Portfolio/stablegraphlayouts.html">Graph layout stability in process mining</see>
    /// </summary>
    static member ComputeGlobalRanking (log: OcelLog) =
        log.ObjectTypes
        |> List.ofSeq
        |> List.map (fun t -> OcelHelpers.Flatten log t |> StableGraphLayout.variationsInLog (Some t))
        |> List.concat
        |> List.sortByDescending StableGraphLayout.importanceSort
        |> StableGraphLayout.computeGlobalRankingForVariations

    /// <summary>
    /// Compute a Global Ranking for all activities in an event log, flattening for a specific object type.
    /// Expects the log to not contain identical objects with different ID's. Use <see cref="OCEL.Types.OcelLog.MergeDuplicateObjects"/> to merge them beforehand.
    /// Based on <see href="https://doi.org/10.1111/cgf.13723">Mennens, R.J.P., Scheepens, R. and Westenberg, M.A. (2019), A stable graph layout algorithm for processes. Computer Graphics Forum, 38: 725-737</see>
    /// and <see href="https://robinmennens.github.io/Portfolio/stablegraphlayouts.html">Graph layout stability in process mining</see>
    /// </summary>
    static member ComputeGlobalRankingForObjectType (log: OcelLog) objectType =
        OcelHelpers.Flatten log objectType
        |> StableGraphLayout.variationsInLog None
        |> List.sortByDescending StableGraphLayout.importanceSort
        |> StableGraphLayout.computeGlobalRankingForVariations

    /// <summary>
    /// Compute a Global Ranking for each object type in an event log.
    /// Expects the log to not contain identical objects with different ID's. Use <see cref="OCEL.Types.OcelLog.MergeDuplicateObjects"/> to merge them beforehand.
    /// Based on <see href="https://doi.org/10.1111/cgf.13723">Mennens, R.J.P., Scheepens, R. and Westenberg, M.A. (2019), A stable graph layout algorithm for processes. Computer Graphics Forum, 38: 725-737</see>
    /// and <see href="https://robinmennens.github.io/Portfolio/stablegraphlayouts.html">Graph layout stability in process mining</see>
    /// </summary>
    static member ComputeGlobalRankingForEachObjectType (log: OcelLog) =
        log.ObjectTypes |> Seq.map (fun ot -> ot, StableGraphLayout.ComputeGlobalRankingForObjectType log ot) |> Map.ofSeq

    /// <summary>
    /// Compute a stable graph layout for the discovered graph.
    /// Based on <see href="https://doi.org/10.1111/cgf.13723">Mennens, R.J.P., Scheepens, R. and Westenberg, M.A. (2019), A stable graph layout algorithm for processes. Computer Graphics Forum, 38: 725-737</see>
    /// </summary>
    static member Compute (graph: DirectedGraph<Node, Edge>) =
        0
