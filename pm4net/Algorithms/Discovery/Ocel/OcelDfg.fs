namespace pm4net.Algorithms.Discovery.Ocel

open System
open OCEL.Types
open pm4net.Types
open pm4net.Types.Trees
open pm4net.Utilities

[<AbstractClass; Sealed>]
type OcelDfg private () =
    
    /// Count the number of occurences of an activity in multiple traces
    static member private noOfEventsWithCase (traces: (string * OcelEvent) seq seq) =
        // For each trace, count the number of distinct activities and accumulate the result into a mapping for all traces
        (Map.empty<string, int>, traces)
        ||> Seq.fold (fun cnt trace ->
            // Count the number of distinct activities in this trace
            let actCount = trace |> Seq.groupBy (fun (_, e) -> e.Activity) |> Seq.map (fun (act, events) -> act, events |> Seq.length)
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

    /// Determine whether a namespace string is inside of a namespace tree
    static member private isNamespaceInTree (ns: string) (tree: ListTree<string>) =

        /// Determine whether a list of string components is in a given list tree
        let rec isInTree cmps tree =
            match cmps with
            | [] -> true
            | cmp :: tail ->
                // Find the correct branch of the current level of the tree, if it exists
                let branch =
                    match tree with
                    | Node(_, children) ->
                        children
                        |> Seq.tryFind (fun c ->
                            match c with
                            | Node(n, _) -> n = cmp)

                // Repeat procedure until no more components left (positive outcome) or no matching branches are left (negative outcome)
                match branch with
                | Some branch -> isInTree tail branch
                | None ->
                    // Check if the tree has exactly one child with a wildcard, in which case all remaining components are allowed
                    match match tree with | Node(_, c) -> c |> Seq.tryExactlyOne with
                    | Some(Node(n, _)) -> n = "*"
                    | None -> false

        let nsComponents = ns.Split('.') |> Array.filter (fun i -> i <> String.Empty) |> List.ofArray
        isInTree nsComponents tree

    /// Calcualte the duration between two events
    static member private durationBetweenEvents (e1, e2) =
        e2.Timestamp - e1.Timestamp

    /// Get the timestamp of the first and last event in a trace
    static member private getFirstAndLastEventTimestamp trace =
        trace |> Seq.head |> fun (_, e) -> e.Timestamp, trace |> Seq.last |> fun (_, e) -> e.Timestamp

    /// Get the traces of a flattened event log, applying the given filters
    static member GetTracesForSingleType(filter, log: OcelLog) : seq<OcelObject * seq<string * OcelEvent>> =
        // Start by discovering the traces based on the referenced object type and discard the event ID's
        let traces = log |> OcelHelpers.OrderedTracesOfFlattenedLog

        // Additional step: Filter for log levels
        let tracesFilteredForLogLevel = traces |> Seq.choose (fun (o, t) ->
            let filtered = t |> Seq.filter (fun (_, e) ->
                match OcelHelpers.GetLogLevel e with
                // If the event doesn't have a log level, the Unknown value needs to be enabled
                | None -> filter.IncludedLogLevels |> Seq.contains LogLevel.Unknown
                | Some l -> filter.IncludedLogLevels |> Seq.contains l)
            if filtered |> Seq.isEmpty then None else Some (o, filtered))

        // Additional step: Filter for timeframe
        let tracesFilteredForTimeframe =
            match filter.Timeframe with
            | Some tf ->
                tracesFilteredForLogLevel |> Seq.choose (fun (o, t) ->
                    let f, l = OcelDfg.getFirstAndLastEventTimestamp t
                    match tf.KeepCases with
                    | ContainedInTimeframe -> if f > tf.From && l < tf.To then Some(o, t) else None
                    // If it starts before, the last one must be in the timeframe or after it. If it starts within the timeframe, all is good.
                    | IntersectingTimeframe -> if (f < tf.From && l >= tf.From) || (f >= tf.From && f <= tf.To) then Some(o, t) else None
                    | StartedInTimeframe -> if f >= tf.From && f <= tf.To then Some(o, t) else None
                    | CompletedInTimeframe -> if l >= tf.From && l <= tf.To then Some(o, t) else None
                    | TrimToTimeframe -> t |> Seq.filter (fun (_, e) -> e.Timestamp >= tf.From && e.Timestamp <= tf.To) |> fun t -> if t |> Seq.isEmpty then None else Some(o, t))
            | None -> tracesFilteredForLogLevel

        // Additional step: Filter for namespaces
        let tracesFilteredForNamespaces =
            match filter.IncludedNamespaces with
            | Some ns ->
                tracesFilteredForTimeframe |> Seq.choose (fun (o, t) ->
                    let filtered = t |> Seq.filter (fun (_, e) ->
                        match OcelHelpers.GetNamespace e with
                        | Some eNs -> ns |> OcelDfg.isNamespaceInTree eNs
                        | None -> true) // Always allow events without namespace through
                    if filtered |> Seq.isEmpty then None else Some(o, filtered))
            | None -> tracesFilteredForTimeframe

        // Step 2: Remove all cases from log having a trace with a frequency lower than minEvents
        let tracesFilteredForLength = tracesFilteredForNamespaces |> Seq.filter (fun (_, v) -> v |> Seq.length >= filter.MinEvents)

        // Step 3: Remove all events with a frequency lower than minOccurrences
        let noOfEvents = OcelDfg.noOfEventsWithCase (tracesFilteredForLength |> Seq.map snd)
        let tracesFilteredForFrequency = tracesFilteredForLength |> Seq.map (fun (o, v) -> o, v |> Seq.filter (fun (_, e) -> Map.find e.Activity noOfEvents >= filter.MinOccurrences))

        tracesFilteredForFrequency
            
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
    static member DiscoverForSingleType(filter, objectType, log: OcelLog) : DirectedGraph<Node<NodeInfo>, Edge<EdgeInfo>> =

        // Compute filtered traces traces
        let traces = OcelDfg.GetTracesForSingleType(filter, log)

        // Step 4: Add a node for each activity remaining in the filtered event log
        let groupedByActivityNamespace = traces |> Seq.collect snd |> Seq.groupBy (fun (_, e) -> e.Activity, OcelHelpers.GetNamespace e)

        // Create map of nodes with activity and optional namespace as key, for efficient lookup down below when creating edges
        let nodes =
            groupedByActivityNamespace
            |> Seq.map (fun ((act, ns), events) ->
                (act, ns), EventNode({
                    Name = act
                    Info = Some {
                        Frequency = events |> Seq.length
                        Namespace = ns
                        Level = events |> Seq.toList |> Helpers.mostCommonValue (fun (_, e) -> OcelHelpers.GetLogLevel e)
                        Attributes = events |> Seq.head |> fun (_, e) -> e.VMap
                        Objects = events |> Seq.head |> fun (_, e) -> e.OMap |> List.map (fun o -> log.Objects[o])
                    }
                }))
            |> Map.ofSeq

        // Alternate version in progress
        // Step 5: Connect the nodes that meet the minSuccessions treshold, i.e. activities a and b are connected if and only if #L''(a,b) >= minSuccessions
        let directlyFollowing =
            traces
            |> Seq.map snd
            |> Seq.collect Seq.pairwise
            |> Seq.groupBy (fun ((_, a), (_, b)) -> (a.Activity, OcelHelpers.GetNamespace a), (b.Activity, OcelHelpers.GetNamespace b))

        let edges =
            (([]: (Node<NodeInfo> * Node<NodeInfo> * Edge<EdgeInfo>) list), directlyFollowing)
            ||> Seq.fold (fun edges (((aAct, aNs), (bAct, bNs)), trace) ->
                let aNode = nodes[aAct, aNs]
                let bNode = nodes[bAct, bNs]
                let durations = trace |> Seq.map (fun ((_, a), (_, b)) -> OcelDfg.durationBetweenEvents(a, b))
                (aNode, bNode, { Weight = trace |> Seq.length; Type = Some objectType; Info = Some { Durations = durations |> Seq.toList } }) :: edges)
            |> Seq.filter (fun (_, _, e) -> e.Weight >= filter.MinSuccessions) // Filter out edges that do not satisfy minimum threshold

        // Find and insert start and stop nodes and their respective edges
        let starts = traces |> Seq.filter (fun (_, l) -> l |> Seq.isEmpty |> not) |> Seq.map (fun (_, t) -> t |> Seq.head) |> Seq.countBy (fun (_, e) -> e.Activity, OcelHelpers.GetNamespace e)
        let ends = traces |> Seq.filter (fun (_, l) -> l |> Seq.isEmpty |> not) |> Seq.map (fun (_, t) -> t |> Seq.last) |> Seq.countBy (fun (_, e) -> e.Activity, OcelHelpers.GetNamespace e)
        let startNode = StartNode(objectType)
        let endNode = EndNode(objectType)
        let nodes = nodes |> Map.add ($"{Constants.objectTypeStartNode}{objectType}", None) startNode
        let nodes = nodes |> Map.add ($"{Constants.objectTypeEndNode}{objectType}", None) endNode
        let edges =
            edges
            |> Seq.append (starts |> Seq.map (fun ((name, ns), count) -> (startNode, nodes[name, ns], { Weight = count; Type = Some objectType; Info = None }))) // Connect all start nodes
            |> Seq.append (ends |> Seq.map (fun ((name, ns), count) -> (nodes[name, ns], endNode, { Weight = count; Type = Some objectType; Info = None }))) // Connect all end nodes

        // Return a directed graph with the discovered nodes and edges
        { Nodes = nodes |> Map.values |> Seq.toList; Edges = edges |> Seq.toList }

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
    static member Discover(filter, includedTypes, log: OcelLog) : DirectedGraph<Node<NodeInfo>, Edge<EdgeInfo>> =
        log.ObjectTypes
        |> Set.filter (fun t -> includedTypes |> List.contains t) // Only include object types from the list in the parameters
        |> Seq.map (fun t ->
            let flattened = OcelHelpers.Flatten log t
            t, OcelDfg.DiscoverForSingleType(filter, t, flattened)) // Flatten the log based on every object type and discover a model for each
        |> Seq.fold (fun state (_, value) ->
            { state with
                Nodes =
                    Seq.append state.Nodes value.Nodes
                    |> Seq.groupBy (fun n ->
                        match n with
                        | EventNode n ->
                            let baseStr = nameof(EventNode) + n.Name
                            match n.Info with
                            | Some info -> baseStr + (Option.defaultValue "" info.Namespace) + (Option.defaultValue LogLevel.Unknown info.Level).ToString()
                            | _ -> baseStr
                        | StartNode n -> nameof(StartNode) + n
                        | EndNode n -> nameof(EndNode) + n)
                    |> Seq.map (fun (_, nodes) -> nodes |> Seq.maxBy (fun n ->
                        match n with
                        | EventNode n ->
                            match n.Info with
                            | Some info -> info.Frequency
                            | _ -> 0
                        | StartNode _ | EndNode _ -> 0 // There should only be one start and end node anyway
                    ))
                Edges = Seq.append state.Edges value.Edges
            }
        ) { Nodes = []; Edges = [] }

    (* --- Overloads for C# OCEL log type --- *)

    static member DiscoverForSingleType(filter, objectType, log: OCEL.CSharp.OcelLog) : DirectedGraph<Node<_>, Edge<_>> =
        OcelDfg.DiscoverForSingleType(filter, objectType, OCEL.CSharp.FSharpConverters.ToFSharpOcelLog log)

    static member Discover(filter, includedTypes : string seq, log: OCEL.CSharp.OcelLog) : DirectedGraph<Node<_>, Edge<_>> =
        OcelDfg.Discover(filter, includedTypes |> List.ofSeq, OCEL.CSharp.FSharpConverters.ToFSharpOcelLog log)
