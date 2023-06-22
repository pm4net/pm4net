namespace pm4net.Utilities

open System
open OCEL.Types
open pm4net.Types
open pm4net.Types.Trees

[<AbstractClass; Sealed>]
type OcelHelpers private () =

    /// Flatten an OCEL log to a traditional event log by chosing an object type.
    /// Reference paper: van der Aalst, Wil MP, and Alessandro Berti. "Discovering object-centric Petri nets." (Definition 4.1)
    static member Flatten (log: OcelLog) objectType =
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
                | objs -> kv.Key, objs |> List.map (fun (o_id, _) -> { kv.Value with OMap = [o_id] }))

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
    static member OrderedTracesOfFlattenedLog (log: OcelLog) =
        log.OrderedEvents
        |> Seq.groupBy (fun (_, v) -> v.OMap |> Seq.head)
        |> Seq.map (fun (k, e) -> log.Objects[k], e)

    /// Create a trace for an object type in order to use it for the stable graph layout algorithm.
    static member TracesForObjectType objType (log: OcelLog) =
        log
        |> OcelHelpers.OrderedTracesOfFlattenedLog
        |> Seq.map (fun (_, l) -> l |> Seq.map (fun (_, e) -> e.Activity) |> Seq.toList)
        |> Seq.countBy id
        |> Seq.distinctBy fst
        |> Seq.map (fun (t, cnt) -> { Events = t |> List.toSeq; Frequency = cnt; Type = objType } : InputTypes.Trace)

    /// Create traces for all object types in order to use it for the stable graph layout algorithm.
    static member AllTracesOfLog (log: OcelLog) =
        log.ObjectTypes
        |> Set.toSeq
        |> Seq.map (fun t -> t |> OcelHelpers.Flatten log |> OcelHelpers.TracesForObjectType (Some t))
        |> Seq.concat

    /// Get an attribute from an OCEL event, if it exists.
    static member TryGetAttribute attr event =
        event.VMap |> Map.tryFind attr

    /// Get a string attribute from an OCEL event. Returns an empty string if it does not exist.
    static member GetStringAttribute attr event =
        (attr, event) ||> OcelHelpers.TryGetAttribute
        |> fun v -> match v with | Some(OcelString s) -> Some s | _ -> None

    /// Get the namespace attribute from an OCEL event. Returns an empty string if it does not exist.
    static member GetNamespace event =
        match event |> OcelHelpers.GetStringAttribute Constants.``namespace`` with
        | Some ns -> Some ns
        | _ -> event |> OcelHelpers.GetStringAttribute Constants.sourceContext

    /// Get the log level attribute from an OCEL event. Returns the Unknown case if it does not exist.
    static member GetLogLevel event =
        match event |> OcelHelpers.GetStringAttribute Constants.level with
        | Some level -> level |> LogLevel.FromString |> Some
        | _ -> None

    /// Extract a tree hierarchy from a list of fully qualified namespaces
    static member NamespaceTree separators (namespaces: string seq) =

        /// Insert a list of sequential values into a tree
        let rec insert tree values =

            /// Get the node and its index in a list of nodes that has a given value, if any exist.
            let hasExistingNodeIndex nodes value =
                match nodes |> List.tryFindIndex (fun (Node(v, _)) -> v = value) with
                | Some i -> Some (nodes[i], i)
                | None -> None

            // Only has the Node type, but that has to be deconstructed first
            match tree with
            | Node(node, children) ->
                // Return the input tree if there are no values left to add.
                // Otherwise recurisvely add each item, stepping one level down into the tree with each value.
                match values with
                | [] -> tree
                | head :: tail ->
                    // Check whether there is already a child with the given value.
                    // If yes, discard the duplicate value and continue with the next value on the existing branch.
                    // If no, add the new value as a child of the current node, with no children of itself, and continue the recursive pattern.
                    match head |> hasExistingNodeIndex children with
                    | Some (n, i) ->
                        let updatedNode = insert n tail
                        Node(node, children |> List.updateAt i updatedNode)
                    | None ->
                        // Create a new child node by recursively adding the remaining values, and then adding it to the existing node.
                        let newNode = insert (Node(head, [])) tail
                        Node(node, newNode :: children)

        // Fold over the different namespaces and build up the tree one after another, starting with a root node with an empty string.
        (Node(String.Empty, []), namespaces)
        ||> Seq.fold (fun tree ns ->
            (ns.Split(separators)
                |> Array.filter (fun i -> i <> String.Empty)
                |> List.ofArray)
            |> insert tree)
