namespace pm4net.Utilities

open System
open OCEL.Types
open pm4net.Types

module internal OcelHelpers =

    /// Flatten an OCEL log to a traditional event log by chosing an object type.
    /// Reference paper: van der Aalst, Wil MP, and Alessandro Berti. "Discovering object-centric Petri nets." (Definition 4.1)
    let Flatten (log: OcelLog) objectType =
        if not log.IsValid then
            failwith "Log is not valid."

        if log.ObjectTypes.Contains objectType |> not then
            failwith $"Object type '{objectType}' is not present in the given log."

        /// Get all events of a log, flattened by the object type.
        /// If an event has no object of the type, it is excluded. If it has multiple, the event is duplicated.
        let flattenEventsByObjectType (log: OcelLog) object_type =
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
        { log with Events = (log, objectType) ||> flattenEventsByObjectType |> collectEventsIntoMapping}

    /// Extract the different traces of a flattened OCEL log, where each event has exactly one object reference.
    /// Traces are identified by comparing the referenced object ID (expects duplicate objects to already be merged).
    /// Returns a list of traces, where each trace is a list of event ID and the actual event.
    let OrderedTracesOfFlattenedLog (log: OcelLog) =
        log.OrderedEvents
        |> List.ofSeq
        |> List.groupBy (fun (_, v) -> v.OMap |> Seq.head)
        |> List.map snd

    /// Get an attribute from an OCEL event, if it exists.
    let TryGetAttribute attr event =
        event.VMap |> Map.tryFind attr

    /// Get a string attribute from an OCEL event. Returns an empty string if it does not exist.
    let GetStringAttribute attr event =
        (attr, event) ||> TryGetAttribute
        |> Option.defaultValue (OcelString String.Empty)
        |> fun v -> match v with | OcelString s -> s | _ -> String.Empty

    /// Get the namespace attribute from an OCEL event. Returns an empty string if it does not exist.
    let GetNamespace event =
        event |> GetStringAttribute "pm4net_Namespace"

    /// Get the log level attribute from an OCEL event. Returns the Unknown case if it does not exist.
    let GetLogLevel event =
        event |> GetStringAttribute "pm4net_Level" |> LogLevel.FromString
