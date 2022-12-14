namespace pm4net.Utilities

module internal OcelUtitilies =

    /// Get a sequence of objects and their ID's that are referenced in an event and match a given object type
    let private getMatchingObjects (log: OCEL.Types.OcelLog) (event: OCEL.Types.OcelEvent) o_type =
        event.OMap
        |> Seq.map (fun o_id -> o_id, log.Objects[o_id])
        |> Seq.filter (fun (_, o) -> o.Type = o_type)

    let private getEventsBasedOnObjects (log: OCEL.Types.OcelLog) o_type =
        log.Events
        |> Seq.map (fun kv ->
            match getMatchingObjects log kv.Value o_type |> List.ofSeq with
            | [] -> []
            | objs -> objs |> List.map (fun (o_id, _) -> { kv.Value with OMap = [o_id] })
        )
        |> Seq.collect id

    /// Flatten an OCEL log to a traditional event log by chosing an object type.
    let flatten (log: OCEL.Types.OcelLog) o_type =
        if not log.IsValid then
            failwith "Log is not valid."

        if log.ObjectTypes.Contains o_type |> not then
            failwith $"Object type '{o_type}' is not present in the given log."

        getEventsBasedOnObjects log "item"
