namespace pm4net.Algorithms.Discovery.Ocel

open pm4net.Types
open pm4net.Utilities

module OcelDirectlyFollowsGraph =

    /// <summary>
    /// Create a Directly-Follows-Graph (DFG) from traces.
    /// Based on <see href="http://www.padsweb.rwth-aachen.de/wvdaalst/publications/p1101.pdf">A practitioner's guide to process mining: Limitations of the directly-follows graph</see> 
    /// </summary>
    /// <param name="traces">A map of case ID's and their respective events</param>
    /// <param name="tVar">Minimal number of events in a trace for it to be included.</param>
    /// <param name="tAct">Minimal number of global occurences for events to be kept in a trace.</param>
    /// <param name="tDf">Minimal number of direct successions for a relationship to be included in the DFG.</param>
    /// <returns>A Directly-Follows Graph from the traces, with the thresholds applied.</returns>
    let discoverFromTraces (traces: OCEL.Types.OcelEvent list list) tVar tAct tDf : DirectlyFollowsGraph =

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
        let tFilteredCases = traces |> List.filter (fun v -> v.Length >= tVar)

        // Step 3: Remove all events with a frequency lower than tAct
        let noOfEvents = noOfEventsWithCase tFilteredCases
        let tRemovedEvents = tFilteredCases |> List.map (fun v -> v |> List.filter (fun e -> Map.find e.Activity noOfEvents >= tAct))

        // Step 4: Add a node for each activity remaining in the filtered event log
        let nodesWithFrequency = noOfEventsWithCase tRemovedEvents

        // Step 5: Connect the nodes that meet the tDf treshold, i.e. activities a and b are connected if and only if #L''(a,b) >= tDf
        let edges =
            (Map.empty<string * string, int>, tRemovedEvents)
            ||> List.fold (fun edges trace ->
                // Get pairs of events that directly follow each other
                let directlyFollowing = trace |> List.pairwise
                // Add or change counter of edge in mapping
                (edges, directlyFollowing) ||> List.fold (fun s v ->
                    let a, b = (fst v).Activity, (snd v).Activity
                    match (a, b) |> s.ContainsKey with
                    | true -> s |> Map.change (a, b) (fun cnt -> match cnt with | Some c -> c + 1 |> Some | None -> None)
                    | false -> s |> Map.add (a, b) 1
                )
            )
            |> Map.filter (fun _ cnt -> cnt >= tDf)

        // Step 6: Return nodes and edges as a tuple
        { Nodes = nodesWithFrequency; Edges = edges }

    /// <summary>
    /// Create a Directly-Follows-Graph (DFG) for each object type in a log.
    /// Based on <see href="http://www.padsweb.rwth-aachen.de/wvdaalst/publications/p1101.pdf">A practitioner's guide to process mining: Limitations of the directly-follows graph</see> 
    /// </summary>
    /// <param name="log">An object-centric event log.</param>
    /// <param name="tVar">Minimal number of events in a trace for it to be included.</param>
    /// <param name="tAct">Minimal number of global occurences for events to be kept in a trace.</param>
    /// <param name="tDf">Minimal number of direct successions for a relationship to be included in the DFG.</param>
    /// <returns>A map of object types to Directly-Follows Graphs for that type, with the thresholds applied.</returns>
    let discoverFromLog (log: OCEL.Types.OcelLog) tVar tAct tDf : Map<string, DirectlyFollowsGraph> =
        let flattenedByTypes = log.ObjectTypes |> Seq.map (fun t -> t, OcelUtitilies.flatten log t) |> Map.ofSeq
        let orderedTraces = flattenedByTypes |> Map.map (fun _ v -> OcelUtitilies.orderedTracesOfFlattenedLog v)
        orderedTraces |> Map.map (fun _ v -> discoverFromTraces (v |> HelperFunctions.mapNestedList snd) tVar tAct tDf)
