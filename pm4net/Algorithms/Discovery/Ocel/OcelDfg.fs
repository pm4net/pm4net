namespace pm4net.Algorithms.Discovery.Ocel

open GraphTypes
open InputTypes
open OCEL.Types
open pm4net.Types
open pm4net.Utilities

[<AbstractClass; Sealed>]
type OcelDfg private () =
    
    /// Count the number of occurences of an activity in multiple traces
    static member private noOfEventsWithCase (traces: OcelEvent seq seq) =
        // For each trace, count the number of distinct activities and accumulate the result into a mapping for all traces
        (Map.empty<string, int>, traces)
        ||> Seq.fold (fun cnt trace ->
            // Count the number of distinct activities in this trace
            let actCount = trace |> Seq.groupBy (fun e -> e.Activity) |> Seq.map (fun (act, events) -> act, events |> Seq.length)
            // Add missing keys to the map if encountering activities that were not seen before, and set the initial value to 0
            let cnt = (cnt, actCount) ||> Seq.fold (fun s v ->
                match fst v |> s.ContainsKey with
                | true -> s
                | false -> s |> Map.add (fst v) 0
            )
            // Add the count of distinct activities from this trace to the entire count, and return it as the new state
            (cnt, actCount) ||> Seq.fold (fun s v ->
                s |> Map.change (fst v) (fun c ->
                    match c with
                    | None -> None
                    | Some c -> Some (c + snd v)
                )
            )
        )

    /// Calcualte the duration between two events
    static member private durationBetweenEvents (e1, e2) =
        e2.Timestamp - e1.Timestamp

    /// Find an event node in a list of nodes by its name and namespace
    static member private findNode name ns (nodes: Node<NodeInfo> seq) =
        nodes
        |> Seq.find (fun n ->
            match n with
            | EventNode e -> e.Name = name && (match e.Info with | Some info -> info.Namespace = ns | _ -> false)
            | _ -> false)

    /// Find an edge in a list of edges by its name and namespace of start and end, and the object type
    static member private findEdge (name1, ns1) (name2, ns2) (edges: (Node<NodeInfo> * Node<NodeInfo> * Edge<EdgeInfo>) seq) =
        edges
        |> Seq.find (fun (a, b, _) ->
            match a, b with
            | EventNode a, EventNode b ->
                a.Name = name1 &&
                b.Name = name2 &&
                (match a.Info with | Some info -> info.Namespace = ns1 | _ -> false) &&
                (match b.Info with | Some info -> info.Namespace = ns1 | _ -> false)
            | _ -> false)

    /// <summary>
    /// Create an Object-Centric Directly-Follows-Graph (DFG) for a flattened log, given the object type of the flattened log and a set of filter parameters.
    /// Expects the log to not contain identical objects with different ID's. Use <see cref="OCEL.Types.OcelLog.MergeDuplicateObjects"/> to merge them beforehand.
    /// Based on <see href="http://www.padsweb.rwth-aachen.de/wvdaalst/publications/p1101.pdf">A practitioner's guide to process mining: Limitations of the directly-follows graph</see> and
    /// <see href="http://www.padsweb.rwth-aachen.de/wvdaalst/publications/p1056.pdf">Object-Centric Process Mining: Dealing with Divergence and Convergence in Event Data.</see>
    /// </summary>
    /// <param name="minEvents">Minimal number of events in a trace for it to be included.</param>
    /// <param name="minOccurrences">Minimal number of global occurences for events to be kept in a trace.</param>
    /// <param name="minSuccessions">Minimal number of direct successions for a relationship to be included in the DFG.</param>
    /// <param name="objectType">The object type after whicht the log was flattened.</param>
    /// <param name="log">An object-centric event log that was flattened for a specific object type.</param>
    /// <returns>An Object-Centric Directly-Follows-Graph (DFG) with the filters applied.</returns>
    static member DiscoverForSingleType(minEvents, minOccurrences, minSuccessions, objectType, log: OcelLog) : DirectedGraph<Node<NodeInfo>, Edge<EdgeInfo>> =
        // Discover the traces based on the referenced object type and discard the event ID's
        let traces = log |> OcelHelpers.OrderedTracesOfFlattenedLog |> Helpers.mapNestedSeq snd

        // Step 2: Remove all cases from log having a trace with a frequency lower than minEvents
        let tracesFilteredForLength = traces |> Seq.filter (fun v -> v |> Seq.length >= minEvents)

        // Step 3: Remove all events with a frequency lower than minOccurrences
        let noOfEvents = OcelDfg.noOfEventsWithCase tracesFilteredForLength
        let tracesFilteredForFrequency = tracesFilteredForLength |> Seq.map (fun v -> v |> Seq.filter (fun e -> Map.find e.Activity noOfEvents >= minOccurrences))

        // Step 4: Add a node for each activity remaining in the filtered event log
        let groupedByActivityNamespace = traces |> Seq.collect id |> Seq.groupBy (fun e -> e.Activity, OcelHelpers.GetNamespace e)
        let nodes = groupedByActivityNamespace |> Seq.map (
            fun ((act, ns), events) -> EventNode(
                {
                    Name = act
                    Info = Some {
                        Frequency = events |> Seq.length
                        Namespace = ns
                        Level = events |> Seq.toList |> Helpers.mostCommonValue (fun e -> OcelHelpers.GetLogLevel e)
                    }
                }
            )
        )
        
        // Step 5: Connect the nodes that meet the minSuccessions treshold, i.e. activities a and b are connected if and only if #L''(a,b) >= minSuccessions
        let edges =
            (([]: (Node<NodeInfo> * Node<NodeInfo> * Edge<EdgeInfo>) seq), tracesFilteredForFrequency)
            ||> Seq.fold (fun edges trace ->
                // Get pairs of events that directly follow each other
                let directlyFollowing = trace |> Seq.pairwise
                // Add or change counter of edge in mapping
                (edges, directlyFollowing) ||> Seq.fold (fun s v ->
                    let aNode = OcelDfg.findNode (fst v).Activity (fst v |> OcelHelpers.GetNamespace) nodes
                    let bNode = OcelDfg.findNode (snd v).Activity (snd v |> OcelHelpers.GetNamespace) nodes
                    match s |> Seq.tryFindIndex (fun (a, b, _) -> a = aNode && b = bNode) with
                    | Some i ->
                        let (a, b, edge) = s |> Seq.item i
                        s |> Seq.updateAt i (a, b,
                        { edge with
                            Weight = edge.Weight + 1
                            Info =
                                match edge.Info with
                                | Some edgeInfo -> Some { edgeInfo with Durations = OcelDfg.durationBetweenEvents v :: edgeInfo.Durations }
                                | _ -> None
                        })
                    | None -> [(aNode, bNode, { Weight = 1; Type = Some objectType; Info = Some { Durations = [OcelDfg.durationBetweenEvents v] } })] |> Seq.append s))
            // Filter out edges that do not satisfy minimum threshold
            |> Seq.filter (fun (_, _, e) -> e.Weight >= minSuccessions)

        // Find and insert start and stop nodes and their respective edges
        let starts = tracesFilteredForFrequency |> Seq.filter (fun l -> l |> Seq.isEmpty |> not) |> Seq.map (fun t -> t |> Seq.head) |> Seq.countBy (fun e -> e.Activity, OcelHelpers.GetNamespace e)
        let ends = tracesFilteredForFrequency |> Seq.filter (fun l -> l |> Seq.isEmpty |> not) |> Seq.map (fun t -> t |> Seq.last) |> Seq.countBy (fun e -> e.Activity, OcelHelpers.GetNamespace e)
        let startNode = StartNode(objectType)
        let endNode = EndNode(objectType)
        let nodes = startNode :: endNode :: (nodes |> Seq.toList)
        let edges =
            edges
            |> Seq.append (starts |> Seq.map (fun ((name, ns), count) -> (startNode, nodes |> OcelDfg.findNode name ns, { Weight = count; Type = Some objectType; Info = None }))) // Connect all start nodes
            |> Seq.append (ends |> Seq.map (fun ((name, ns), count) -> (nodes |> OcelDfg.findNode name ns, endNode, { Weight = count; Type = Some objectType; Info = None }))) // Connect all end nodes

        // Return a directed graph with the discovered nodes and edges
        { Nodes = nodes; Edges = edges |> Seq.toList }

    /// <summary>
    /// Create an Object-Centric Directly-Follows-Graph (DFG) for a log, given a set of filter parameters.
    /// Expects the log to not contain identical objects with different ID's. Use <see cref="OCEL.Types.OcelLog.MergeDuplicateObjects"/> to merge them beforehand.
    /// Based on <see href="http://www.padsweb.rwth-aachen.de/wvdaalst/publications/p1101.pdf">A practitioner's guide to process mining: Limitations of the directly-follows graph</see> and
    /// <see href="http://www.padsweb.rwth-aachen.de/wvdaalst/publications/p1056.pdf">Object-Centric Process Mining: Dealing with Divergence and Convergence in Event Data.</see>
    /// </summary>
    /// <param name="minEvents">Minimal number of events in a trace for it to be included.</param>
    /// <param name="minOccurrences">Minimal number of global occurences for events to be kept in a trace.</param>
    /// <param name="minSuccessions">Minimal number of direct successions for a relationship to be included in the DFG.</param>
    /// <param name="includedTypes">A list of strings with the object types to include in the DFG.</param>
    /// <param name="log">An object-centric event log.</param>
    /// <returns>An Object-Centric Directly-Follows-Graph (DFG) with the filters applied.</returns>
    static member Discover(minEvents, minOccurrences, minSuccessions, includedTypes, log: OcelLog) : DirectedGraph<Node<NodeInfo>, Edge<EdgeInfo>> =
        log.ObjectTypes
        |> Set.filter (fun t -> includedTypes |> List.contains t) // Only include object types from the list in the parameters
        |> Seq.map (fun t -> t, OcelHelpers.Flatten log t) // Flatten the log based on every object type
        |> Map.ofSeq // Create a map of object types to flattened log
        |> Map.map (fun objType v -> OcelDfg.DiscoverForSingleType(minEvents, minOccurrences, minSuccessions, objType, v)) // Discover DFG for each type individually
        |> Map.fold (fun state _ value -> // Merge the DFG's for each type together
            { state with
                Nodes =
                    List.append state.Nodes value.Nodes
                    |> List.groupBy (fun n ->
                        match n with
                        | EventNode n ->
                            let baseStr = nameof(EventNode) + n.Name
                            match n.Info with
                            | Some info -> baseStr + (Option.defaultValue "" info.Namespace) + (Option.defaultValue LogLevel.Unknown info.Level).ToString()
                            | _ -> baseStr
                        | StartNode n -> nameof(StartNode) + n
                        | EndNode n -> nameof(EndNode) + n)
                    |> List.map (fun (_, nodes) -> nodes |> List.maxBy (fun n ->
                        match n with
                        | EventNode n ->
                            match n.Info with
                            | Some info -> info.Frequency
                            | _ -> 0
                        | StartNode _ | EndNode _ -> 0 // There should only be one start and end node anyway
                    ))
                Edges = List.append state.Edges value.Edges
            }
        ) { Nodes = []; Edges = [] } 

    (* --- Overloads for C# OCEL log type --- *)

    static member DiscoverForSingleType(minEvents, minOccurrences, minSuccessions, objectType, log: OCEL.CSharp.OcelLog) : DirectedGraph<Node<_>, Edge<_>> =
        OcelDfg.DiscoverForSingleType(minEvents, minOccurrences, minSuccessions, objectType, OCEL.CSharp.FSharpConverters.ToFSharpOcelLog log)

    static member Discover(minEvents, minOccurrences, minSuccessions, includedTypes : string seq, log: OCEL.CSharp.OcelLog) : DirectedGraph<Node<_>, Edge<_>> =
        OcelDfg.Discover(minEvents, minOccurrences, minSuccessions, includedTypes |> List.ofSeq, OCEL.CSharp.FSharpConverters.ToFSharpOcelLog log)
