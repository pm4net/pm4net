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
