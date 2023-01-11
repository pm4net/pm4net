namespace pm4net.Algorithms.Discovery.Ocel

open pm4net.Types
open pm4net.Utilities

module OcelDirectlyFollowsGraph =

    /// <summary>
    /// Create a Directly-Follows-Graph (DFG) from traces.
    /// Based on <see href="http://www.padsweb.rwth-aachen.de/wvdaalst/publications/p1101.pdf">A practitioner's guide to process mining: Limitations of the directly-follows graph</see> 
    /// </summary>
    /// <param name="tVar">Minimal number of events in a trace for it to be included.</param>
    /// <param name="tAct">Minimal number of global occurences for events to be kept in a trace.</param>
    /// <param name="tDf">Minimal number of direct successions for a relationship to be included in the DFG.</param>
    /// <param name="objType">The name of the object type of this trace. Used to insert start and end node.</param>
    /// <param name="traces">A map of case ID's and their respective events</param>
    /// <returns>A Directly-Follows Graph from the traces, with the thresholds applied.</returns>
    let discoverFromTraces tVar tAct tDf objType (traces: OCEL.Types.OcelEvent list list) : DirectedGraph<DfgNode, DfgEdge> =

        /// Count the number of occurences of an activity in multiple traces
        let noOfEventsWithCase (traces: OCEL.Types.OcelEvent list list) =
            // For each trace, count the number of distinct activities and accumulate the result into a mapping for all traces
            (Map.empty<string, int>, traces)
            ||> List.fold (fun cnt trace ->
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

        // Step 2: Remove all cases from log having a trace with a frequency lower than tVar
        let tracesFilteredForLength = traces |> List.filter (fun v -> v.Length >= tVar)

        // Step 3: Remove all events with a frequency lower than tAct
        let noOfEvents = noOfEventsWithCase tracesFilteredForLength
        let tracesFilteredForFrequency = tracesFilteredForLength |> List.map (fun v -> v |> List.filter (fun e -> Map.find e.Activity noOfEvents >= tAct))

        // Step 4: Add a node for each activity remaining in the filtered event log
        let grouped = traces |> List.collect id |> List.groupBy (fun e -> e.Activity)
        let nodes =
            grouped
            |> List.map (fun (act, events) ->
                EventNode(
                    {
                        Name = act
                        Frequency = events.Length
                        Level = LogLevel.Information // TODO
                    }
                )
            )

        /// Function to quickly find an event node with a given name
        let findNode name nodes = nodes |> List.find (fun n -> match n with | EventNode e -> e.Name = name | _ -> false)

        // Step 5: Connect the nodes that meet the tDf treshold, i.e. activities a and b are connected if and only if #L''(a,b) >= tDf
        let edges =
            (([]: (DfgNode * DfgNode * DfgEdge) list), tracesFilteredForFrequency)
            ||> List.fold (fun edges trace ->
                // Get pairs of events that directly follow each other
                let directlyFollowing = trace |> List.pairwise
                // Add or change counter of edge in mapping
                (edges, directlyFollowing) ||> List.fold (fun s v ->
                    let aNode = findNode (fst v).Activity nodes
                    let bNode = findNode (snd v).Activity nodes
                    match s |> List.tryFindIndex (fun (a, b, _) -> a = aNode && b = bNode) with
                    | Some i ->
                        let (a, b, (name, freq)) = s[i]
                        s |> List.updateAt i (a, b, (name, freq + 1))
                    | None -> (aNode, bNode, (objType, 1)) :: s
                )
            )
            // Filter out edges that do not satisfy minimum threshold
            |> List.filter (fun (_, _, (_, freq)) -> freq >= tDf)

        // Find and insert start and stop nodes and their respective edges
        let starts = tracesFilteredForFrequency |> List.map (fun t -> t.Head) |> List.countBy (fun e -> e.Activity)
        let ends = tracesFilteredForFrequency |> List.map (fun t -> t |> List.last) |> List.countBy (fun e -> e.Activity)
        let startNode = StartNode(objType)
        let endNode = EndNode(objType)
        let nodes = startNode :: endNode :: nodes
        let edges = edges |> List.append (starts |> List.map (fun (name, count) -> (startNode, nodes |> findNode name, (objType, count))))
        let edges = edges |> List.append (ends |> List.map (fun (name, count) -> (nodes |> findNode name, endNode, (objType, count))))

        // Step 6: Return nodes and edges as a Directed Graph
        { Nodes = nodes; Edges = edges }

    /// <summary>
    /// Create a Directly-Follows-Graph (DFG) for each object type in a log.
    /// Based on <see href="http://www.padsweb.rwth-aachen.de/wvdaalst/publications/p1101.pdf">A practitioner's guide to process mining: Limitations of the directly-follows graph</see> 
    /// </summary>
    /// <param name="tVar">Minimal number of events in a trace for it to be included.</param>
    /// <param name="tAct">Minimal number of global occurences for events to be kept in a trace.</param>
    /// <param name="tDf">Minimal number of direct successions for a relationship to be included in the DFG.</param>
    /// <param name="includedObjectTypes">A list of strings with the object types to include in the DFG.</param>
    /// <param name="log">An object-centric event log.</param>
    /// <returns>A map of object types to Directly-Follows Graphs for that type, with the thresholds applied.</returns>
    let discoverFromLog tVar tAct tDf removeDuplicates includedObjectTypes (log: OCEL.Types.OcelLog) : DirectedGraph<DfgNode, DfgEdge> =
        // Merge possible duplicate objects before continuing, if desired. This might be undesired if the Object ID itself carries important information.
        let log = if removeDuplicates then log.MergeDuplicateObjects() else log

        log.ObjectTypes
        |> Set.filter (fun t -> includedObjectTypes |> List.contains t) // Only include object types from the list in the parameters
        |> Seq.map (fun t -> t, OcelUtitilies.flatten log t) // Flatten the log based on every object type
        |> Map.ofSeq // Create a map of object types to flattened log
        |> Map.map (fun _ v -> OcelUtitilies.orderedTracesOfFlattenedLog v) // Extract the individual traces of each object type's flattened log, based on the referenced object
        |> Map.map (fun k v -> discoverFromTraces tVar tAct tDf k (v |> HelperFunctions.mapNestedList snd)) // Discover a DFG for each object type based on the discovered traces
        |> Map.fold (fun state _ value -> // Merge the DFG's for each type together
            { state with
                Nodes =
                    // If there are any duplicate nodes in the new value, choose the one with the maximum frequency (instead of e.g. summing the frequencies)
                    List.append state.Nodes value.Nodes
                    |> List.groupBy (fun n ->
                        match n with
                        | EventNode n -> n.Name
                        | StartNode n -> nameof(StartNode) + n
                        | EndNode n -> nameof(EndNode) + n
                    )
                    |> List.map (fun (_, nodes) -> nodes |> List.maxBy (fun n ->
                        match n with
                        | EventNode n -> n.Frequency
                        | StartNode _ | EndNode _ -> 0 // There should only be one start and end node anyway
                    ))
                Edges = List.append state.Edges value.Edges
            }
        ) { Nodes = []; Edges = [] } 
