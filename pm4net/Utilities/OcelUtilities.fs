namespace pm4net.Utilities

module OcelUtitilies =

    /// Flatten an OCEL log to a traditional event log by chosing an object type.
    let flatten (log: OCEL.Types.OcelLog) object_type =
        if not log.IsValid then
            failwith "Log is not valid."

        if log.ObjectTypes.Contains object_type |> not then
            failwith $"Object type '{object_type}' is not present in the given log."

        /// Get all events of a log, flattened by the object type.
        /// If an event has no object of the type, it is excluded. If it has multiple, the event is duplicated.
        let flattenEventsByObjectType (log: OCEL.Types.OcelLog) object_type =
            log.Events
            |> Seq.map (fun kv ->
                let objs = 
                    kv.Value.OMap
                    |> Seq.map (fun o_id -> o_id, log.Objects[o_id])
                    |> Seq.filter (fun (_, o) -> o.Type = object_type)
                match objs |> List.ofSeq with
                | [] -> kv.Key, []
                | objs -> kv.Key, objs |> List.map (fun (o_id, _) -> { kv.Value with OMap = [o_id] })
            )

        /// Collect a sequence of event id's and multiple corresponding events into a single mapping of event id's and events, appending a number to the id to make it unique (if more than 1 event for same id exists).
        let collectEventsIntoMapping events =
            events
            |> Seq.map (fun (id, events) ->
                match events with
                | [] -> []
                | [e] -> [id, e]
                | e -> e |> List.mapi (fun i e -> $"{id}-{i+1}", e))
            |> Seq.collect id
            |> Map.ofSeq

        // Return the same log, with the events replaced by the flattened events
        { log with Events = (log, object_type) ||> flattenEventsByObjectType |> collectEventsIntoMapping}

    /// Extract the different traces of a flattened OCEL log, where each event has exactly one object reference.
    /// Traces are identified by comparing the referenced object by equality, even if they do not have the same ID.
    /// Returns a list of traces, where each trace is a list of event ID and the actual event.
    let orderedTracesOfFlattenedLog (log: OCEL.Types.OcelLog) =
        log.OrderedEvents
        |> List.ofSeq
        |> List.groupBy (fun (_, v) -> log.Objects[v.OMap |> Seq.head])
        |> List.map snd

    /// <summary>
    /// Create a Directly-Follows-Graph (DFG) from traces.
    /// Based on <see href="http://www.padsweb.rwth-aachen.de/wvdaalst/publications/p1101.pdf">A practitioner's guide to process mining: Limitations of the directly-follows graph</see> 
    /// </summary>
    /// <param name="traces">A map of case ID's and their respective events</param>
    /// <param name="tVar"></param>
    /// <param name="tAct"></param>
    /// <param name="tDf"></param>
    /// <returns></returns>
    let directlyFollowsGraph (traces: (string * OCEL.Types.OcelEvent) list list) tVar tAct tDf =

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

        // Remove the ID of the event, as it it not relevant here
        let traces = traces |> List.map (fun trace -> trace |> List.map snd)

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
        nodesWithFrequency, edges
