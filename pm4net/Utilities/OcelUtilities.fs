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
    /// Returns a mapping from case identifiers of the object type to a mapping of event ID's and the actual events.
    let tracesOfFlattenedLog (log: OCEL.Types.OcelLog) =
        log.Events
        |> Map.toList
        |> List.groupBy (fun (_, v) -> v.OMap |> Seq.head)
        |> List.map (fun (k, v) -> k, Map.ofList v)
        |> Map.ofList

    /// <summary>
    /// Create a Directly-Follows-Graph (DFG) from traces.
    /// Based on http://www.padsweb.rwth-aachen.de/wvdaalst/publications/p1101.pdf
    /// </summary>
    /// <param name="traces">A map of case ID's and their respective events</param>
    /// <param name="tVar"></param>
    /// <param name="tAct"></param>
    /// <param name="tDf"></param>
    /// <returns></returns>
    let directlyFollowsGraph (traces: Map<string, Map<string, OCEL.Types.OcelEvent>>) tVar tAct tDf =
        /// Count the number of occurences of an activity in multiple traces
        let noOfEventsWithCase (traces: Map<string, Map<string, OCEL.Types.OcelEvent>>) =
            // For each trace, count the number of distinct activities and accumulate the result into a mapping for all traces
            (Map.empty<string, int>, traces)
            ||> Map.fold (fun cnt _ trace ->
                // Count the number of distinct activities in this trace
                let actCount = trace.Values |> Seq.groupBy (fun e -> e.Activity) |> Seq.map (fun (act, events) -> act, events |> Seq.length)
                // Add missing keys to the map if encountering activities that were not seen before, and set the initial value to 0
                let cnt = (cnt, actCount) ||> Seq.fold (fun s v ->
                    match fst v |> s.ContainsKey with
                    | true -> s
                    | false -> s |> Map.add (fst v) 0
                )
                // Add the count of distinct activities from thsi trace to the entire count, and return it as the new state
                (cnt, actCount) ||> Seq.fold (fun s v ->
                    s |> Map.change (fst v) (fun c ->
                        match c with
                        | None -> None
                        | Some c -> Some (c + snd v)
                    )
                )
            )

        // Step 2: Remove all cases from log having a trace with a frequency lower than tVar
        let tFilteredCases = traces |> Map.filter (fun _ v -> v.Count >= tVar)

        // Step 3: Remove all events with a frequency lower than tAct
        let noOfEventsWithCase = noOfEventsWithCase tFilteredCases
        let tRemovedEvents = tFilteredCases |> Map.map (fun _ v -> v |> Map.filter (fun _ e -> Map.find e.Activity noOfEventsWithCase >= tAct))

        // Step 4: Add a node for each activity remaining in the filtered event log
        // TODO
        0

    let relationsFootprint (traces: Map<string, Map<string, OCEL.Types.OcelEvent>>) =
        0
