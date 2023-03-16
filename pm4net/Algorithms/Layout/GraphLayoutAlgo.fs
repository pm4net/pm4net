namespace pm4net.Algorithms.Layout

open pm4net.Types
open pm4net.Types.Graphs
open pm4net.Types.GraphLayout

/// An implementation of the "stable graph layout algorithm for processes" introduced by Mennens 2019 (https://doi.org/10.1111/cgf.13723), implemented by Johannes Mols.
module internal GraphLayoutAlgo =

    /// Update a node's rank in a rank graph, changing the edges that reference the nodes with it
    let private updateNode (rankGraph: GlobalRankGraph) node newRank =
        let nodeIdx = rankGraph.Nodes |> List.findIndex (fun n -> fst n = node)
        let outEdges = rankGraph.Edges |> List.indexed |> List.filter (fun (_, ((a, _), _, _)) -> a = node) |> List.map (fun (i, ((a, _), b, freq)) -> i, ((a, newRank), b, freq))
        let inEdges = rankGraph.Edges |> List.indexed |> List.filter (fun (_, (_, (b, _), _)) -> b = node) |> List.map (fun (i, (a, (b, _), freq)) -> i, (a, (b, newRank), freq))
        { rankGraph with
            Nodes = rankGraph.Nodes |> List.updateAt nodeIdx (node, newRank)
            Edges = (rankGraph.Edges, outEdges @ inEdges) ||> List.fold (fun s (i, e) -> s |> List.updateAt i e)
        }

    /// Insert a sequence of edges and nodes that have not been seen before in the graph
    let private insertSequence (rankGraph: GlobalRankGraph) components sequence =

        /// Get the rank of a node with a given name
        let rankOfNode (rankGraph: GlobalRankGraph) node =
            rankGraph.Nodes |> List.find (fun n -> fst n = node) |> snd

        /// Get the lowest rank encountered so far for any node
        let lowestRank (rankGraph: GlobalRankGraph) =
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
                        let nRank, vRank = n |> rankOfNode rankGraph, v |> rankOfNode rankGraph
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
                    (node, (rankOfNode graph node) + numRanks) ||> updateNode graph
                else graph
            )

        /// Merge two previously unconnected components together (Algorithm 2 from Mennens 2018)
        let mergeComponents rankGraph (components: Set<string> list) a b offset =
            let rankA = rankOfNode rankGraph a
            let rankB = rankOfNode rankGraph b
            let dist = rankA - rankB + offset
            let rankGraph = (rankGraph, components[b |> findContainingComponentIndex components]) ||> Set.fold (fun graph node ->
                updateNode graph node ((rankOfNode graph node) + dist))
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
                | Edge (a, b) -> ((a, a |> rankOfNode graph), (b, b |> rankOfNode graph), freq) |> addEdge graph components
                | _ -> graph, components)

        /// Insert a Type 2 sequence (single edge) into a global rank graph
        let insertSingleEdge rankGraph components (a, b, freq) =
            let rankA = rankOfNode rankGraph a
            let rankB = rankOfNode rankGraph b
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
            let rankU, rankV = rankOfNode rankGraph u, rankOfNode rankGraph v
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
                        shiftNodes rankGraph y v (rankOfNode rankGraph y - rankV + 1), components
                    else rankGraph, components
                else
                    let rankGraph, components = insertSequenceIntoGraph (fun r -> r - 1) rankGraph components (rankU - 1) seq
                    let freeRanks = rankU - rankV - 1
                    if noOfNodes > freeRanks then
                        shiftNodes rankGraph v y (rankV - rankOfNode rankGraph y + 1), components
                    else rankGraph, components

        match sequence with
        | [Node _, _] -> insertSequenceIntoGraph (fun r -> r + 1) rankGraph components (rankGraph |> lowestRank) sequence // Type 1
        | [Edge (a, b), freq] -> insertSingleEdge rankGraph components (a, b, freq) // Type 2
        | _ ->
            match sequence, sequence |> List.rev |> List.head with
            | (Node _, _) :: _, (Edge (_ , dest), _) -> insertSequenceIntoGraph (fun r -> r - 1) rankGraph components ((dest |> rankOfNode rankGraph) - 1) (sequence |> List.rev) // Type 3
            | (Edge (orig, _), _) :: _, (Node _, _) -> insertSequenceIntoGraph (fun r -> r + 1) rankGraph components ((orig |> rankOfNode rankGraph) + 1) sequence // Type 4
            | (Node _, _) :: _, (Node _, _) -> insertSequenceIntoGraph (fun r -> r + 1) rankGraph components (rankGraph |> lowestRank) sequence // Type 5
            | (Edge _, _) :: _, (Edge _, _) -> insertEdgeToEdge rankGraph components sequence // Type 6
            | _ -> rankGraph, components

    /// Normalize the ranks of a rank graph so that the first node is always on rank 0 (can become negative during processing), and so that there are no gaps in the ranks
    let private normalizeRanks (rankGraph: GlobalRankGraph) =
        let nodes = rankGraph.Nodes |> List.sortBy snd
        let currentRank = match nodes |> List.tryHead with | Some(_, r) -> r | _ -> 0
        ((rankGraph, currentRank, 0), nodes) ||> List.fold (fun (rankGraph, currentRank, targetRank) (node, rank) ->
            if rank = currentRank then
                if rank <> targetRank then updateNode rankGraph node targetRank, currentRank, targetRank
                else rankGraph, currentRank, targetRank
            else updateNode rankGraph node (targetRank + 1), rank, targetRank + 1
        ) |> fun (rg, _, _) -> rg

    /// Get the rank of a sequence node, real or virtual
    let private getRank elem =
        match elem with
        | Real(rank, _, _)
        | Virtual(rank, _) -> rank

    /// Get the discovery index of a sequence node, real or virtual
    let private getDiscoveryIndex elem =
        match elem with
        | Real(_, idx, _)
        | Virtual(_, idx) -> idx

    /// Get the name of a sequence node, if it is real
    let private getName elem =
        match elem with
        | Real(_, _, name) -> Some name
        | _ -> None

    /// Get the name of the DFG node as it would have been added to the global rank graph
    let private getNodeName node =
        match node with
        | EventNode node -> node.Name
        | StartNode objType -> Constants.objectTypeStartNode + objType
        | EndNode objType -> Constants.objectTypeEndNode + objType

    /// Sort traces based on their importance (sum of w^2 * |v|^2)
    let internal importanceSort variation =
        let wSquaredSum = variation.Sequence |> List.sumBy (fun s -> match s with | Node _, _ -> 0L | Edge _, freq -> pown (freq |> int64) 2)
        let vLenSquared = pown (variation.Events.Length |> int64) 2
        wSquaredSum * vLenSquared

    /// Compute a global ranking, process skeleton, and connected components for a list of variations, according to the Algorithm from Mennens 2018 & 2019
    let internal computeGlobalRankingAndSkeletonForVariations variations =

        /// Reverse the order of nodes and edges in the graph, as they are added to the beginning of the list when discovering (purely for intuitiveness)
        let reverseNodeAndEdgeOrder rankGraph : DirectedGraph<_,_> =
            { rankGraph with Nodes = rankGraph.Nodes |> List.rev; Edges = rankGraph.Edges |> List.rev }

        /// Update the edges that occur multiple times in the variation, after they have been added to the rank graph
        let updateDuplicateEdges rankGraph variation =
            // Find edges in the variation that already exist in the rank graph, but also appear at least twice in the variation
            let duplicateEdges =
                variation.Sequence
                |> List.choose (fun se ->
                    match se with
                    | Edge(a, b), freq ->
                        match rankGraph.Edges |> List.tryFindIndex (fun ((edgeA, _), (edgeB, _), _) -> a = edgeA && b = edgeB) with
                        | Some idx -> Some (a, b, idx, freq)
                        | _ -> None
                    | _ -> None)
                |> List.groupBy (fun (a, b, _, _) -> a, b)
                |> List.map (fun (_, edges) ->
                    match edges |> List.skip 1 with
                    | [] -> None
                    | edges ->
                        edges
                        |> List.reduce (fun (a, b, idx, freqA) (_, _, _, freqB) -> a, b, idx, freqA + freqB)
                        |> fun (_, _, idx, freq) -> Some(idx, freq))
                |> List.choose id

            // Update edges with aggregated frequency
            (rankGraph, duplicateEdges) ||> List.fold (fun graph (idx, freqToIncrement) ->
                let (a, b, existingFreq) = graph.Edges[idx]
                { graph with Edges = graph.Edges |> List.updateAt idx (a, b, existingFreq + freqToIncrement) })

        /// Extract a new continuous sequence that only contains nodes and/or edges that are not yet in the rank graph. Stops when duplicates are encountered. (Mennens 2019, Section 3.1.1)
        let newSequence rankGraph variation =

            /// Checks whether a given sequence element is already in the rank graph
            let existsInRankGraph (rg: GlobalRankGraph) = function
                | Node n, _ -> rg.Nodes |> List.exists (fun (act, _) -> act = n)
                | Edge (a, b), _ -> rg.Edges |> List.exists (fun ((actA, _), (actB, _), _) -> actA = a && actB = b)

            /// Add sequence elements until a duplicate is encountered, either already in the rank graph, or in the new sequence
            let rec addUntilDuplicate rankGraph state (seen: Set<_>) seq =
                match seq with
                | [] -> state |> List.rev
                | elem :: rest ->
                    if elem |> existsInRankGraph rankGraph then state |> List.rev
                    else if elem |> seen.Contains then elem :: state |> List.rev // Add the duplicate element to "complete" sequence, since it's not in rank graph yet
                    else addUntilDuplicate rankGraph (elem :: state) (seen.Add elem) rest
            
            // Try to find the index of the first sequence element that hasn't already been added to the rank graph, if any
            match variation.Sequence |> List.tryFindIndex (fun elem -> elem |> existsInRankGraph rankGraph |> not) with
            | None -> []
            | Some idx -> ([], Set.empty, variation.Sequence |> List.skip idx) |||> addUntilDuplicate rankGraph

        /// Process a variation until no more new sequences can be added to the rank graph
        let rec processVariation variation skeleton (rankGraph, components) =
            match (rankGraph, variation) ||> newSequence with
            | [] -> rankGraph, components, skeleton
            | seq ->
                // Insert the first found new sequence into the graph
                let rankGraph, components = seq |> insertSequence rankGraph components
                // Add the sequence to the skeleton if it contains at least one node (Definition 4.2.1 of Mennens 2018)
                let skeleton = if seq |> List.exists (fun elem -> match elem with | Node _, _ -> true | _ -> false) then skeleton @ [seq] else skeleton
                // Process the variation again until there are no more new sequences available in the variation
                (rankGraph, components) |> processVariation variation skeleton

        // Discover the final rank graph and the skeleton thereof, when adding all variations to an empty rank graph
        let rankGraph, components, skeleton =
            (({ Nodes = []; Edges = [] }, [], []), variations)
            ||> List.fold (fun (graph, components, skeleton) variation ->
                let graph, components, skeleton = (graph, components) |> processVariation variation skeleton
                let graph = updateDuplicateEdges graph variation
                graph, components, skeleton)

        // Normalize the rank graph and order nodes and edges correctly, and return it together with the skeleton
        rankGraph |> normalizeRanks |> reverseNodeAndEdgeOrder, skeleton, components

    /// Check the edges of a discovered model, and fix any horizontal edges that might be present due to filtering (Algorithm 6 from Mennens 2018)
    let internal fixHorizontalEdgesInGlobalRankGraphForDiscoveredModel (rankGraph: GlobalRankGraph) components model =

        let horizontalEdgesByRank =
            model.Edges
            |> List.map (fun (a, b, e) ->
                let nodeA = rankGraph.Nodes |> List.find (fun (n, _) -> n = getNodeName a)
                let nodeB = rankGraph.Nodes |> List.find (fun (n, _) -> n = getNodeName b) 
                (a, b, e), (nodeA, nodeB))
            |> List.filter (fun (_, (nodeA, nodeB)) -> (nodeA = nodeB) |> not && snd nodeA = snd nodeB)
            |> List.groupBy (fun (_, (a, _)) -> snd a) // Group by rank
            |> List.map (fun (_, v) -> v)

        // For each horizontal edge found, insert the single-edge sequence into the global rank graph to fix it
        ((rankGraph, components), horizontalEdgesByRank) ||> List.fold (fun (graph, components) horizontalEdges ->
            let edgesSortedbyFrequency = horizontalEdges |> List.sortByDescending (fun ((_, _, e), _) -> e.Statistics.Frequency)
            ((graph, components), edgesSortedbyFrequency) ||> List.fold (fun (graph, components) ((a, b, edge), _) ->
                let seq = [Edge(getNodeName a, getNodeName b), edge.Statistics.Frequency]
                seq |> insertSequence graph components))
        |> fun (rg, comp) -> rg |> normalizeRanks, comp

    /// Compute a node sequence graph given a global rank graph and the process skeleton
    let internal computeNodeSequenceGraph rankGraph skeleton =

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
                | Virtual(r, idx) -> r = rank && idx = discoveryIndex)

        /// Try to find a real node on a rank with a given name, disregarding the discovery index
        let tryFindRealNodeOnRank (nsg: NodeSequenceGraph) rank name =
            nsg.Nodes |> List.tryFindIndex (fun n ->
                match n with
                | Real(r, _, n) -> r = rank && n = name
                | _ -> false)

        /// Add a virtual node to the NSG, if there isn't already an existing node at this rank with the same discovery index
        let addVirtualNodeIfNotExists nsg rank discoveryIndex =
            let virtualNode = Virtual(rank, discoveryIndex)
            match tryFindNode nsg rank discoveryIndex with
            | Some nodeIndex -> nsg, nsg.Nodes[nodeIndex]
            | None -> { nsg with Nodes = virtualNode :: nsg.Nodes }, virtualNode

        /// Add a real node to the NSG, replacing any virtual nodes that may already be there. Also updates any edges that reference the replaced node.
        let addOrReplaceRealNode nsg rank discoveryIndex name =
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
            | _ -> nsg, nextNode // All done!

        (({ Nodes = []; Edges = [] }: DirectedGraph<_>), skeleton |> List.indexed) ||> List.fold (fun nsg (idx, seq) ->
            let edges = seq |> List.filter (fun elem -> match elem with | Edge _, _ -> true | _ -> false)
            (nsg, edges) ||> List.fold (fun nsg edge ->
                match edge with
                | Edge (a, b), _ ->
                    /// Get the rank of a node with a given name
                    let rankOfNode (rankGraph: GlobalRankGraph) node =
                        rankGraph.Nodes |> List.find (fun n -> fst n = node) |> snd

                    // Find the ranks of A and B in the global rank graph
                    let rankA, rankB = rankOfNode rankGraph a, rankOfNode rankGraph b

                    // Add the real nodes of the edge origin and destination if they don't already exist, or replace
                    let nsg, nodeA = addOrReplaceRealNode nsg rankA idx a
                    let nsg, nodeB = addOrReplaceRealNode nsg rankB idx b

                    // Determine whether the target node is in the same "column", i.e. has the same discovery index. If not, the last edge must merge the connection
                    let targetA, targetB = getDiscoveryIndex nodeA, getDiscoveryIndex nodeB

                    // Add a connection between the two nodes, and insert virtual nodes where required
                    match abs(rankA - rankB) with
                    | 1 -> addEdgeIfNotExists nsg nodeA nodeB |> fst
                    | _ ->
                        match targetA = targetB with
                        | true -> addVirtualNodesAndEdgesBetweenRanks nsg rankA nodeA rankB idx |> fst
                        | false ->
                            // Only add virtual nodes up to the rank just before the target, and then connect it to the actual target node via an edge
                            let nsg, nodeToConnect = addVirtualNodesAndEdgesBetweenRanks nsg rankA nodeA (if rankA - rankB >= 0 then rankB + 1 else rankB - 1) idx
                            addEdgeIfNotExists nsg nodeToConnect nodeB |> fst
                | _ -> nsg))

    /// Compute a global order given a global rank graph and a node sequence graph
    let internal computeGlobalOrder (rankGraph: GlobalRankGraph) (nsg: NodeSequenceGraph) =

        /// Get the list of nodes belonging to the same sequence as a given node, sorted by their rank 
        let nodeSequence (nsg: NodeSequenceGraph) (node: SequenceNode) =
            let nodeDiscoveryIdx = getDiscoveryIndex node
            nsg.Nodes
            |> List.filter (fun n ->
                match n with
                | Real(_, idx, _)
                | Virtual(_, idx) -> idx = nodeDiscoveryIdx)
            |> List.sortBy (fun n -> getRank n)

        /// Compute the sequence connectedness of two sequences (Definition 4.2.4 of Mennens 2018)
        let sequenceConnectedness (rg: GlobalRankGraph) (nsg: NodeSequenceGraph) (s1: SequenceNode list) (s2: SequenceNode list) =
            nsg.Edges
            // Only get edges that start in one node sequence and end in the other
            |> List.filter (fun (a, b) -> List.contains a s1 && List.contains b s2 || List.contains b s1 && List.contains a s2)
            |> List.sumBy (fun (a, b) ->
                match getName a, getName b with
                | Some a, Some b -> rg.Edges |> List.find (fun ((edgeA, _), (edgeB, _), _) -> a = edgeA && b = edgeB) |> fun (_, _, freq) -> freq
                | _ -> 0)

        /// Get a sort value for a given node based on the backbone connectedness of its node sequence
        let connectednessSort (nsg: NodeSequenceGraph) backboneSeqs node =
            let nodeSequence = node |> nodeSequence nsg
            backboneSeqs |> List.sumBy (fun seq -> sequenceConnectedness rankGraph nsg seq nodeSequence)

        /// Find the connected components when excluding backbone nodes
        let findComponents (nsg: NodeSequenceGraph) backbone =

            /// Find the list of connected nodes, excluding nodes that are part of the backbone
            let rec findConnectedNodes (nsg: NodeSequenceGraph) (backbone: SequenceNode list) visited node =
                let visited = node :: visited
                let connectedNodes =
                    nsg.Edges
                    |> List.filter (fun (a, b) ->
                        backbone |> List.contains node |> not &&
                        (a = node && backbone |> List.contains b |> not && visited |> List.contains b |> not) ||
                        (b = node && backbone |> List.contains a |> not && visited |> List.contains a |> not))
                    |> List.map (fun (a, b) -> if a = node then b else a)
                let visited = (visited, connectedNodes) ||> List.fold (fun vis node -> (vis, node) ||> findConnectedNodes nsg backbone)
                visited |> List.distinct

            // Find all nodes that we need to visit (all except backbone), and fold through them, finding their components along the way
            let nodesToVisit = backbone |> Set.ofList |> Set.difference (nsg.Nodes |> Set.ofList) |> Set.toList
            ([], nodesToVisit) ||> List.fold (fun components node ->
                match List.exists (fun comp -> comp |> List.contains node) components with
                | true -> components // Node already in a component
                | false ->
                    let nodes = ([], node) ||> findConnectedNodes nsg backbone
                    if nodes.IsEmpty then components else nodes :: components)
            |> List.sortByDescending (fun set -> set.Length)

        /// Calculate the ratio of nodes that are left or right of the backbone in a global order NSG
        let calculateRatio (goNsg: GlobalOrderNodeSequenceGraph) (backbone: SequenceNode list) =
            let (left, right) =
                goNsg.Nodes
                |> List.filter (fun (_, n) -> backbone |> List.contains n |> not)
                |> List.partition (fun (x, _) -> x < 0)

            (left.Length |> float32) / (right.Length |> float32)

        /// Creates a global order graph from a node sequence graph and the sort order permutation for each rank
        let createGlobalOrderNsg (nsg: NodeSequenceGraph) sortedNodes : GlobalOrderNodeSequenceGraph =
            let nodesWithPos = sortedNodes |> List.map (fun (_, nodes) -> nodes |> List.indexed) |> List.concat
            let edgesWithPos = nsg.Edges |> List.map (fun (a, b) -> nodesWithPos |> List.find (fun (_, n) -> a = n), nodesWithPos |> List.find (fun (_, n) -> b = n))
            { Nodes = nodesWithPos; Edges = edgesWithPos }

        /// Move a component to the left side of the backbone of a global order graph, preserving the connectedness sort order
        let moveComponentLeft (goNsg: GlobalOrderNodeSequenceGraph) (originalOrder: (int * SequenceNode list) list) (nodes: SequenceNode list) =

            /// Update any edge that references a node with the new position assigned to the node
            let updateEdge (goNsg: GlobalOrderNodeSequenceGraph) ((pos, node): int * SequenceNode) =
                let edges = goNsg.Edges |> List.indexed |> List.filter (fun (_, ((_, a), (_, b))) -> a = node || b = node)
                let updatedEdges = (goNsg.Edges, edges) ||> List.fold (fun state (idx, ((posA, a), (posB, b))) ->
                    let newEdge = if a = node then ((pos, a), (posB, b)) else ((posA, a), (pos, b))
                    state |> List.updateAt idx newEdge)
                { goNsg with Edges = updatedEdges }

            /// Close any gaps on the right-hand side created by moving a node to the left side of the backbone
            let closeGaps rank (goNsg: GlobalOrderNodeSequenceGraph) =
                let updatedNodesOnRight =
                    goNsg.Nodes
                    |> List.indexed
                    |> List.filter (fun (_, (pos, n)) -> pos > 0 && n |> getRank = rank)
                    |> List.sortBy (fun (_, (pos, _)) -> pos)
                    |> List.mapi (fun i (idx, (_, node)) -> idx, (i + 1, node)) // Assign position based on iteration index + 1, discarding previous position

                let updatedNodes = (goNsg.Nodes, updatedNodesOnRight) ||> List.fold (fun state (idx, node) -> state |> List.updateAt idx node)
                ({ goNsg with Nodes = updatedNodes }, updatedNodesOnRight) ||> List.fold (fun graph (_, node) -> updateEdge graph node)

            (goNsg, nodes) ||> List.fold (fun graph newNode ->
                /// Get the index in the original ordering of a given node on a rank
                let getIdxInOriginalOrdering originalOrder rank node =
                    originalOrder |> List.find (fun (r, _) -> r = rank) |> snd |> List.findIndex (fun n -> n = node)

                // Find the index of the node to move in the existing graph so that it can later be updated
                let newNodeIdx = graph.Nodes |> List.findIndex (fun (_, n) -> n = newNode)
                let newNodeRank = getRank newNode

                // Find the nodes on the same rank that already are on the left side of the backbone
                let nodesLeftOnSameRank =
                    graph.Nodes
                    |> List.indexed
                    |> List.filter (fun (_, (x, n)) -> x < 0 && getRank n = getRank newNode)

                // If there is an existing node with a lower order index, place the new node to the left of it and shift all other nodes one to the left
                // Otherwise, the node can be placed right next to the backbone at position -1
                match nodesLeftOnSameRank with
                | [] ->
                    let newNodeWithPos = (-1, newNode)
                    let graph = { graph with Nodes = graph.Nodes |> List.updateAt newNodeIdx newNodeWithPos } |> closeGaps newNodeRank
                    updateEdge graph newNodeWithPos
                | leftNodes ->
                    // First partition nodes by whether they will be on the left side of the node to move, or on the right, based on the original connectedness ordering for the rank
                    let (left, right) = leftNodes |> List.partition (fun (_, (_, node)) -> getIdxInOriginalOrdering originalOrder newNodeRank newNode < getIdxInOriginalOrdering originalOrder newNodeRank node)

                    // Find the lowest position of nodes on the right-hand side and subtract -1 to place it to the left of that node
                    let newNodePos =
                        match right with
                        | [] -> -1
                        | right -> right |> List.minBy (fun (_, (pos, _)) -> pos) |> fun (_, (pos, _)) -> pos - 1 // Place left of node with lowest position

                    // Shift nodes on the left one position to the left
                    let left = left |> List.map (fun (idx, (pos, node)) -> idx, (pos - 1, node))

                    // Update the nodes left of the moved node and the moved note itself with the new position
                    let nodesToUpdate = (newNodeIdx, (newNodePos, newNode)) :: left
                    let updatedNodes = (graph.Nodes, nodesToUpdate) ||> List.fold (fun state (idx, nodeToUpdate) -> state |> List.updateAt idx nodeToUpdate)
                    ({ graph with Nodes = updatedNodes } |> closeGaps newNodeRank, nodesToUpdate) ||> List.fold (fun graph (_, node) -> updateEdge graph node))

        /// Balance the NSG by moving components to the left of the backbone, if that improves the balance.
        let balanceComponents (goNsg: GlobalOrderNodeSequenceGraph) (components: SequenceNode list list) (backbone: SequenceNode list) (originalOrder: (int * SequenceNode list) list) : GlobalOrderNodeSequenceGraph =
            (goNsg, components) ||> List.fold (fun graph comp ->
                let ratioBefore = (graph, backbone) ||> calculateRatio
                let ratioAfter = ({ graph with Nodes = graph.Nodes |> List.map (fun (x, n) -> if comp |> List.contains n then -1, n else x, n)}, backbone) ||> calculateRatio
                let ratioImproved = abs(ratioAfter - 1f) < abs(ratioBefore - 1f) // Is the distance to ideal ratio (1) lower than before?
                if ratioImproved then moveComponentLeft graph originalOrder comp else graph)

        // The list of nodes that belong to the backbone
        let backbone =
            nsg.Nodes
            |> List.groupBy getRank
            |> List.map (fun (_, nodes) -> nodes |> List.minBy getDiscoveryIndex)

        // The set of sequences belonging to the backbone
        let backboneSequences = backbone |> List.map (fun n -> n |> nodeSequence nsg)

        // The nodes on each rank, sorted by backbone connectedness
        let nodesByRankSorted =
            nsg.Nodes
            |> List.groupBy getRank
            |> List.sortBy fst
            |> List.map (fun (rank, nodes) ->
                // Secondary sort key sorts based on discovery index if connectedness sort has the same result. Lower discovery indices are preferred.
                let sorted = nodes |> List.sortByDescending (fun n -> connectednessSort nsg backboneSequences n, -(getDiscoveryIndex n))
                let backboneNodeIdx = sorted |> List.findIndex (fun n -> backbone |> List.contains n)
                rank, sorted |> List.permute (fun i ->
                    if i = backboneNodeIdx then 0 // Move the backbone node back to the front
                    else if i < backboneNodeIdx then i + 1 // If the node was ordered below the backbone node, add one since it is moved down
                    else i)) // Was right of backbone node anyway, nothing to do

        // Convert the existing NSG into a global order graph by inserting X position into nodes
        let globalOrderNsg = createGlobalOrderNsg nsg nodesByRankSorted

        // The connected components when "cutting" the backbone nodes out
        let components = findComponents nsg backbone

        // Balance the components by moving some to the left
        balanceComponents globalOrderNsg components backbone nodesByRankSorted

    /// Construct the actual discovered graph by applying the discovered model to the global order, minimising edge crossings as the graph is discovered
    let internal constructDiscoveredGraph (goNsg: GlobalOrderNodeSequenceGraph) (skeleton: Skeleton) (model: DirectedGraph<Graphs.Node, Graphs.Edge>) : DiscoveredGraph =

        /// Close any gaps in the list of real and virtual nodes that may occur due to filtering
        let closeGaps (nodes: (int * SequenceNode) list) =
            ([], nodes |> List.groupBy (fun n -> snd n |> getRank)) ||> List.fold (fun s (_, nodes) ->
                let leftOfBackbone = nodes |> List.filter (fun (x, _) -> x < 0)
                let rightOfBackbone = nodes |> List.filter (fun (x, _) -> x > 0)
                let backboneNode = nodes |> List.tryFind (fun (x, _) -> x = 0)
                let sortedLeft = leftOfBackbone |> List.rev |> List.mapi (fun i (_, n) -> -i - 1, n) |> List.rev
                let sortedRight = rightOfBackbone |> List.mapi (fun i (_, n) -> i + 1, n)
                match backboneNode with
                | Some n -> (sortedLeft @ [n] @ sortedRight) |> List.append s
                | None -> (sortedLeft @ sortedRight) |> List.append s)

        let nodes =
            goNsg.Nodes
            |> List.filter (fun (_, n) ->
                // Only keep nodes that are also in discovered model, and for now also all virtual nodes
                match n with
                | Virtual _ -> true
                | Real(_, _, name) -> model.Nodes |> List.exists (fun n -> getNodeName n = name))
            |> closeGaps

        let sequenceNodes = nodes |> List.map (fun (x, n) ->
            match n with
            | Real(rank, idx, name) -> ConstrainedReal({ X = float32 x; Y = rank }, idx, name)
            | Virtual(rank, idx) -> ConstrainedVirtual({ X = float32 x; Y = rank }, idx))

        // Start constructing the graph by adding edges
        let graph = ({ Nodes = sequenceNodes; Edges = [] }, model.Edges) ||> List.fold (fun graph (a, b, edge) ->

            /// Find a real constrained node by its normal node counterpart
            let findNode (graph: DiscoveredGraph) name =
                graph.Nodes |> List.find (fun n -> match n with | ConstrainedReal(_, _, n) -> n = name | _ -> false)

            /// Find a node in the new graph from a sequence node
            let findNodeFromSeqNode (graph: DiscoveredGraph) = function
                | Real(_, _, name) -> findNode graph name
                | Virtual(rank, vIdx) -> graph.Nodes |> List.find (fun n ->
                    match n with
                    | ConstrainedVirtual(pos, idx) -> pos.Y = rank && vIdx = idx
                    | _ -> false) // Unconstrained virtual nodes not supported, since they are not yet present in the graph and cannot unqiuely identified by the given sequence node.

            /// Extract the position value from any of the node types
            let getPosition = function
                | ConstrainedReal(pos, _, _)
                | ConstrainedVirtual(pos, _)
                | UnconstrainedVirtual(pos, _) -> pos

            /// Check whether an edge is constrained by checking whether it is present in the skeleton (not always accurate when it contains back-edges to previously encountered nodes)
            let isConstrained (a: Graphs.Node) (b: Graphs.Node) (skeleton: Skeleton) =
                skeleton |> List.exists (fun seq -> seq |> List.exists (fun (e, _) ->
                    match e with
                    | Edge(aName, bName) -> getNodeName a = aName && getNodeName b = bName
                    | _ -> false))

            /// Discover virtual nodes, in order, that lead to target node
            let findVirtualNodesToDestination (goNsg: GlobalOrderNodeSequenceGraph) a b =

                /// Traverse the global order NSG until the target node is found
                let rec traverseUntilTargetFoundOrNot (goNsg: GlobalOrderNodeSequenceGraph) visited current target =
                    let visited = current :: visited
                    let foundTarget = goNsg.Edges |> List.exists (fun ((_, a), (_, b)) -> a = current && b = target || b = current && a = target)
                    if foundTarget then
                        Some visited
                    else
                        let next = goNsg.Edges |> List.choose (fun ((_, a), (_, b)) ->
                            let candidate = if a = current then Some b else if b = current then Some a else None
                            match candidate with
                            | Some candidate ->
                                match candidate with
                                | Virtual _ -> if visited |> List.contains candidate |> not then Some candidate else None
                                | _ -> None
                            | _ -> None)

                        match next with
                        | [next] -> traverseUntilTargetFoundOrNot goNsg visited next target // Continue traversing until we hopefully find the target
                        | _ -> None // Ran out of new nodes to process, meaning that we were on the wrong path

                let discIdxA, discIdxB = getDiscoveryIndex a, getDiscoveryIndex b
                goNsg.Nodes
                |> List.filter (fun (_, n) -> // Get the possible starting points to start traversing the global order NSG
                    match n with
                    | Virtual(_, idx) -> goNsg.Edges |> List.exists (fun ((_, tmpA), (_, tmpB)) ->
                        (a = tmpA && n = tmpB || a = tmpB && n = tmpA) && // Is there a virtual node that the start point is pointing to/from? (NSG is not directed)
                        (idx = discIdxA || idx = discIdxB)) // Does the virtual node have either the discovery index of A or B?
                    | _ -> false)
                |> List.choose (fun (_, start) -> traverseUntilTargetFoundOrNot goNsg [] start b) // Traverse each starting point to see whether it leads to the target
                |> List.tryHead // Choose the first value as there should only be one, and only one

            /// Add a constrained edge to the graph, finding and using any constrained virtual nodes when the edge spans more than one rank
            let addConstrainedEdge a b edge graph =
                let nameA, nameB = getNodeName a, getNodeName b
                let nodeA, nodeB = findNode graph nameA, findNode graph nameB
                let posA, posB = getPosition nodeA, getPosition nodeB
                match abs(posA.Y - posB.Y) with
                | 1 -> { graph with Edges = (nodeA, nodeB, edge.Statistics.Frequency) :: graph.Edges }, true
                | _ ->
                    let (_, nsgNodeA) = goNsg.Nodes |> List.find (fun (_, n) -> match n with | Real(_, _, name) -> name = nameA | _ -> false)
                    let (_, nsgNodeB) = goNsg.Nodes |> List.find (fun (_, n) -> match n with | Real(_, _, name) -> name = nameB | _ -> false)
                    let nodesOnPath = findVirtualNodesToDestination goNsg nsgNodeA nsgNodeB
                    match nodesOnPath with
                    | Some nodesOnPath ->
                        (graph, nsgNodeA :: nodesOnPath @ [nsgNodeB] |> List.pairwise) ||> List.fold (fun graph (start, target) ->
                            { graph with Edges = (start |> findNodeFromSeqNode graph, target |> findNodeFromSeqNode graph, edge.Statistics.Frequency) :: graph.Edges }) |> fun g -> g, true
                    | None -> graph, false // Adding the edge was not successful, likely because it isn't actually constrainted. Return false to add the edge as unconstrained instead.

            /// Add an unconstrained edge to the graph, adding unconstrained virtual nodes if it spans multiple ranks
            let addUnconstrainedEdge a b edge graph =

                /// Add a sequence of virtual unconstrained nodes with an initial ordering between two constrained nodes
                let rec addVirtualNodesBetweenRanks (graph: DiscoveredGraph) (nodeA, nameA, posA: Position) (nodeB, nameB, posB: Position) edgeWeight =
                    let upwards = posA.Y - posB.Y >= 0 // Whether the direction is upwards or downwards
                    let nextRank = posA.Y + if upwards then -1 else 1 // The next rank to consider in the given direction
                    match nextRank = posB.Y with
                    | false ->
                        let xPos = if posA.X < posB.X then posB.X - 0.5f else posB.X + 0.5f
                        let nodePos : Position = { X = xPos; Y = nextRank }
                        let unconstrainedNode = UnconstrainedVirtual(nodePos, { A = nameA; B = nameB })
                        let graph = { graph with Nodes = unconstrainedNode :: graph.Nodes; Edges = (nodeA, unconstrainedNode, edgeWeight) :: graph.Edges }
                        addVirtualNodesBetweenRanks graph (unconstrainedNode, nameA, nodePos) (nodeB, nameB, posB) edgeWeight
                    | true -> { graph with Edges = (nodeA, nodeB, edgeWeight) :: graph.Edges }

                let nameA, nameB = getNodeName a, getNodeName b
                let nodeA, nodeB = findNode graph nameA, findNode graph nameB
                let posA, posB = getPosition nodeA, getPosition nodeB
                match nameA = nameB with
                | true -> graph
                | _ -> 
                    match abs(posA.Y - posB.Y) with
                    | 1 -> { graph with Edges = (nodeA, nodeB, edge.Statistics.Frequency) :: graph.Edges }
                    | _ -> addVirtualNodesBetweenRanks graph (nodeA, nameA, posA) (nodeB, nameB, posB) edge.Statistics.Frequency

            match skeleton |> isConstrained a b with
            | true ->
                // Check whether the edge could successfully be added as a constrained edge, add it as unconstrained if it failed
                match graph |> addConstrainedEdge a b edge with
                | graph, true -> graph
                | graph, false -> graph |> addUnconstrainedEdge a b edge
            | false -> graph |> addUnconstrainedEdge a b edge)

        graph // TODO: Now remove unused virtual nodes and close gaps again (or first do that at the end anyway, before determining x positions of unconstrained virtual nodes)

    /// Minimize edge crossings for a discovered model
    let internal minimizeEdgeCrossings goNsg skeleton model =

        /// Convert a global order NSG to a temporary NSG for crossing minimisation, adding non-sequence nodes and edges from the discovered model
        let insertNodesAndEdges goNsg skeleton model =

            /// Add existing nodes and edges from the global order node sequence graph to the modified datatype which may contain non-sequence nodes and edges
            let addFromGoNsg (goNsg: GlobalOrderNodeSequenceGraph) : CrossMinNsg =
                { Nodes = goNsg.Nodes |> List.map (fun (x, n) -> Sequence(x, n)); Edges = goNsg.Edges |> List.map (fun ((ax, an), (bx, bn)) -> (Sequence(ax, an), Sequence(bx, bn), true)) }

            /// Add non-sequence edges to cross-min node sequence graph
            let addEdges edges goNsg =

                /// Find a real sequence node in the CrossMinNsg with a given name, and return the node and its rank
                let findRealSequenceNode (goNsg: CrossMinNsg) nodeName =
                    goNsg.Nodes |> List.pick (fun n ->
                        match n with
                        | Sequence(_, seqN) ->
                            match seqN with
                            | Real(rank, _, name) -> if name = nodeName then Some(n, rank, name) else None
                            | _ -> None
                        | _ -> None)

                /// Recursively add a number of non-sequence virtual nodes in between two ranks
                let rec addVirtualNodesBetweenRanks (goNsg: CrossMinNsg) (nodeA, rankA, nameA) (nodeB, rankB, nameB) =
                    let upwards = rankA - rankB >= 0 // Whether the direction is upwards or downwards
                    let nextRank = rankA + if upwards then -1 else 1 // The next rank to consider in the given direction
                    match nextRank = rankB with
                    | false ->
                        let virtualNode = NonSequence(nextRank, nameA, nameB)
                        let goNsg = { goNsg with Nodes = virtualNode :: goNsg.Nodes; Edges = (nodeA, virtualNode, false) :: goNsg.Edges }
                        addVirtualNodesBetweenRanks goNsg (virtualNode, nextRank, nameA) (nodeB, rankB, nameB)
                    | true -> { goNsg with Edges = (nodeA, nodeB, false) :: goNsg.Edges }

                (goNsg, edges) ||> List.fold (fun goNsg (a, b, _) ->
                    let (nodeA, rankA, nameA) = getNodeName a |> findRealSequenceNode goNsg
                    let (nodeB, rankB, nameB) = getNodeName b |> findRealSequenceNode goNsg
                    if nameA <> nameB then
                        match abs(rankA - rankB) with
                        | 1 -> { goNsg with Edges = (nodeA, nodeB, false) :: goNsg.Edges }
                        | _ -> addVirtualNodesBetweenRanks goNsg (nodeA, rankA, nameA) (nodeB, rankB, nameB)
                    else goNsg)

            /// Get the edges in the discovered model that are not part of any node sequence, as known from the process skeleton that was discovered earlier
            let edgesNotInRankGraph skeleton model =
                model.Edges |> List.filter (fun (a, b, _) ->
                    let aName, bName = getNodeName a, getNodeName b
                    skeleton |> List.exists (fun nodeSeq ->
                        nodeSeq |> List.exists (fun (node, _) ->
                            match node with
                            | Edge(a, b) -> a = aName && b = bName
                            | _ -> false
                        )) |> not)

            let missingEdges = edgesNotInRankGraph skeleton model
            addFromGoNsg goNsg |> addEdges missingEdges

        // Go through all the nodes in the model, find them in the global order NSG and assign initial x positions in the interval [-1,1]
        let initialOrder (goNsg: CrossMinNsg) : CrossMinNsgWithPos =

            /// Calculate the X position for nodes in the range of [-1, 1], based on their X position and how many there are on this side of the backbone
            let calculateXPos (nodes: CrossMinNode list) =
                nodes
                |> List.sortBy (fun n -> match n with | Sequence(x, _) -> x | _ -> failwith "No non-sequence nodes allowed here")
                |> List.map (fun n -> match n with | Sequence(x, n) -> (float32(x) / float32(nodes.Length + 1), Sequence(x,n)) | _ -> failwith "No non-sequence nodes allowed here")

            /// Find the real sequence node based on the name
            let findRealNode (graph: CrossMinNsgWithPos) nodeName =
                graph.Nodes |> List.pick (fun (initVal, n) ->
                    match n with
                    | Sequence(x, n) ->
                        let name = getName n
                        if name.IsSome && name.Value = nodeName then Some(x, initVal, n) else None
                    | _ -> None)

            /// Add missing edges by finding nodes with assigned positions and re-adding them to the graph
            let addEdges (edges: (CrossMinNode * CrossMinNode * bool) list) (graph: CrossMinNsgWithPos) =
                (graph, edges) ||> List.fold (fun graph (a, b, constrained) ->
                    let aNode = graph.Nodes |> List.find (fun n -> snd n = a)
                    let bNode = graph.Nodes |> List.find (fun n -> snd n = b)
                    { graph with Edges = (aNode, bNode, constrained) :: graph.Edges })

            /// Add a non-sequence node to the cross-min graph
            let addNonSequenceNodes (nodes: CrossMinNode list) (graph: CrossMinNsgWithPos) =

                /// Calculate the X position of a non-sequence node based on Mennens 2019, Section 3.4
                let calculateNonSeqXPos (graph: CrossMinNsgWithPos) (node: CrossMinNode) =
                    match node with
                    | Sequence _ -> failwith "Sequence node not allowed here"
                    | NonSequence(rank, a, b) ->
                        let (xposA, initValA, nodeA), (xposB, initValB, nodeB) = findRealNode graph a, findRealNode graph b
                        let rankA, rankB = getRank nodeA, getRank nodeB

                        if initValA > initValB then initValA
                        else if initValA <= initValB && xposA <> 0 && xposB <> 0 then initValB
                        else if xposA = 0 && xposB = 0 && rankA > rankB then 0.001f else -0.001f

                (graph, nodes) ||> List.fold (fun graph node ->
                    match node with
                    | Sequence _ -> graph
                    | NonSequence _ -> { graph with Nodes = (node |> calculateNonSeqXPos graph, node) :: graph.Nodes })

            /// Get the rank of a node in cross min form
            let getCrossMinRank = function
                | Sequence(_, n) -> getRank n
                | NonSequence(r, _, _) -> r

            // Group real and virtual nodes by rank, and then assign values between -1 and 1 to them
            let nodesByRank = goNsg.Nodes |> List.groupBy (fun n -> getCrossMinRank n)
            (({ Nodes = []; Edges = [] }: CrossMinNsgWithPos), nodesByRank) ||> List.fold (fun state (_, nodes) ->
                let sequenceNodes = nodes |> List.filter (fun n -> match n with | Sequence _ -> true | _ -> false)
                let backboneNode = sequenceNodes |> List.minBy (fun n -> match n with | Sequence(_, n) -> getDiscoveryIndex n | _ -> failwith "No non-sequence nodes allowed here")
                let state = { state with Nodes = (0f, backboneNode) :: state.Nodes } // Add backbone node with value 0
                let sequenceNodes = sequenceNodes |> List.removeAt (sequenceNodes |> List.findIndex (fun n -> n = backboneNode)) // Remove from list to avoid duplicate
                let (left, right) = sequenceNodes |> List.partition (fun n -> match n with | Sequence(x, _) -> x < 0 | _ -> failwith "No non-sequence nodes allowed here")
                let state = { state with Nodes = state.Nodes |> List.append (calculateXPos left) }
                let state = { state with Nodes = state.Nodes |> List.append (calculateXPos right) }
                state) |> addNonSequenceNodes goNsg.Nodes |> addEdges goNsg.Edges

        let sortOrder (graph: CrossMinNsgWithPos) =
            // TODO: Sort nodes based on assigned position, and sort non-sequence virtual nodes based on heuristic (longer edges outermost, if same length then edge weight)
            graph

        insertNodesAndEdges goNsg skeleton model |> initialOrder |> sortOrder

    /// Convert a completed global order graph into a more friendly format for consumers
    let internal convertGlobalOrderToFriendlyFormat (graph: GlobalOrderNodeSequenceGraph) =

        let realFilter node = match node with | Real _ -> true | _ -> false

        /// Virtual nodes are essentially waypoints for edges to navigate when drawing an edge between two nodes. Therefore, find all edges that use a sequence of waypoints and put them together.
        let rec getVirtualComponents (graph: GlobalOrderNodeSequenceGraph) (visited: Set<SequenceNode>) (node: SequenceNode)  =
            let visited = visited.Add node
            let (realNodes, virtualNodes) =
                graph.Edges
                |> List.choose (fun ((_, a), (_, b)) -> if a = node then Some b else if b = node then Some a else None)
                |> List.filter (fun n -> visited |> Set.contains n |> not)
                |> List.partition realFilter

            let visited = visited |> Set.union (realNodes |> Set.ofList)
            (visited, virtualNodes) ||> List.fold (fun vis node -> (vis, node) ||> getVirtualComponents graph)

        /// Get the X,Y coordinates of a sequence node in the global order graph
        let getCoordinatesOfNode (graph: GlobalOrderNodeSequenceGraph) (node: SequenceNode) =
            let (x, n) = graph.Nodes |> List.find (fun (_, n) -> n = node)
            { X = x; Y = n |> getRank }

        /// Merge multiple reachability sets together by seeing whether any virtual nodes are in multiple sets, and join those together
        let mergeReachabilitySets (sets: Set<SequenceNode> list) =
            (([]: Set<SequenceNode> list), sets) ||> List.fold (fun sets set ->
                let (_, virtualNodes) = set |> Set.partition realFilter
                let matchingSetIdx = sets |> List.tryFindIndex (fun s -> virtualNodes |> Set.exists (fun node -> s.Contains node))
                match matchingSetIdx with
                | Some matchingSetIdx -> (set |> Set.union sets[matchingSetIdx] , sets) ||> List.updateAt matchingSetIdx
                | None -> set :: sets)

        /// Create combinations from elements within a list (https://stackoverflow.com/a/1231711/2102106)
        let rec combinations num list =
            match num, list with
            | 0, _ -> [[]]
            | _, [] -> []
            | num, (head :: tail) -> List.map ((@) [head]) (combinations (num - 1) tail) @ combinations num tail

        let nodes = graph.Nodes |> List.choose (fun (x, n) -> match n with | Real(y, _, name) -> Some { Name = name; Position = { X = x; Y = y } } | _ -> None)
        let edgePaths =
            graph.Nodes
            |> List.map snd
            |> List.choose (fun n -> match n with | Virtual _ -> Some n | _ -> None)
            |> List.map (fun n -> getVirtualComponents graph Set.empty n)
            |> mergeReachabilitySets
            |> List.map (fun set ->
                let (realNodes, virtualNodes) = set |> Set.partition realFilter
                let possibleEdges = combinations 2 (realNodes |> Set.toList)
                possibleEdges |> List.map (fun edge ->
                    match edge with
                    | a :: b :: [] ->
                        {
                            Edge = (getName a).Value, (getName b).Value;
                            Waypoints = virtualNodes |> Set.map (fun n -> n |> getCoordinatesOfNode graph) |> Set.toList
                        }
                    | _ -> failwith "Edge may only contain 2 nodes"))
            |> List.concat

        { Nodes = nodes; EdgePaths = edgePaths }
