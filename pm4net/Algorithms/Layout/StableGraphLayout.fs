namespace pm4net.Algorithms.Layout

open System.Runtime.CompilerServices
open OCEL.Types
open pm4net.Types
open pm4net.Types.Graphs
open pm4net.Types.GraphLayout
open pm4net.Algorithms.Layout
open pm4net.Utilities

[<assembly: InternalsVisibleTo("pm4net.Tests")>]
do()

[<AbstractClass; Sealed>]
type StableGraphLayout private () =

    /// Extract the unique variations in the flattened log and order them by importance
    static member private variationsInLog objType log =

        /// Removes direct repetitions in a list (e.g. [A,A,B,C,C] -> [A,B,C]). Section 2.1 of Mennens 2018.
        let removeDirectRepetitions comparer trace =
            ([], trace) ||> List.fold (fun state nextValue ->
                match state |> List.tryLast with
                | None -> [nextValue]
                | Some last -> if comparer last nextValue then state else state @ [nextValue])

        /// Compute the sequence of a trace, not accounting for the existing global rank graph (interleaved nodes and edges) (Definition 4.1.2 of Mennes 2018)
        let simpleSequence freq trace =
            match trace with
            | [] -> []
            | [single] -> [Node(single), freq]
            | _ -> 
                trace
                |> List.pairwise
                |> List.mapi (fun i (a, b) ->
                    match i with
                    | 0 -> [Node(a), 0; Edge(a, b), freq; Node(b), 0]
                    | _ -> [Edge(a, b), freq; Node(b), 0])
                |> List.concat

        log
        |> OcelHelpers.OrderedTracesOfFlattenedLog // Get traces based on referenced object
        |> Helpers.mapNestedList snd // Discard the event ID as it is not relevant
        |> List.map (fun events -> events |> removeDirectRepetitions (fun a b -> a.Activity = b.Activity)) // Remove direct repetitions of events with the same activity
        |> List.map (fun events ->
            // Add a start and end node for the object type to each trace, if it is specified
            match objType with
            | Some objType ->
                let startEvent = { Activity = $"{Constants.objectTypeStartNode} {objType}"; Timestamp = (events |> List.head |> fun e -> e.Timestamp.AddTicks(-1)); OMap = []; VMap = Map.empty }
                let endEvent = { Activity = $"{Constants.objectTypeEndNode} {objType}"; Timestamp = (events |> List.last |> fun e -> e.Timestamp.AddTicks(1)); OMap = []; VMap = Map.empty }
                startEvent :: events @ [endEvent]
            | None -> events)
        |> List.countBy (fun t -> t |> List.map (fun e -> e.Activity)) // Extract only activity name and count the occurrences of each variant/path
        |> List.map (fun (t, cnt) -> { Events = t; Frequency = cnt; Sequence = (cnt, t) ||> simpleSequence })

    /// <summary>
    /// Compute a Global Ranking for all activities in an event log, merging the flattened logs of all object types together to get a total view.
    /// Expects the log to not contain identical objects with different ID's. Use <see cref="OCEL.Types.OcelLog.MergeDuplicateObjects"/> to merge them beforehand.
    /// </summary>
    static member internal computeGlobalRanking (log: OcelLog) =
        log.ObjectTypes
        |> List.ofSeq
        |> List.map (fun t -> OcelHelpers.Flatten log t |> StableGraphLayout.variationsInLog (Some t))
        |> List.concat
        |> List.sortByDescending GraphLayoutAlgo.importanceSort
        |> GraphLayoutAlgo.computeGlobalRankingAndSkeletonForVariations

    /// <summary>
    /// Compute a Global Ranking for all activities in an event log, flattening for a specific object type.
    /// Expects the log to not contain identical objects with different ID's. Use <see cref="OCEL.Types.OcelLog.MergeDuplicateObjects"/> to merge them beforehand.
    /// </summary>
    static member internal computeGlobalRankingForObjectType (log: OcelLog) objectType =
        OcelHelpers.Flatten log objectType
        |> StableGraphLayout.variationsInLog None
        |> List.sortByDescending GraphLayoutAlgo.importanceSort
        |> GraphLayoutAlgo.computeGlobalRankingAndSkeletonForVariations

    /// <summary>
    /// Compute a Global Ranking for each object type in an event log.
    /// Expects the log to not contain identical objects with different ID's. Use <see cref="OCEL.Types.OcelLog.MergeDuplicateObjects"/> to merge them beforehand.
    /// </summary>
    static member internal computeGlobalRankingForEachObjectType (log: OcelLog) =
        log.ObjectTypes |> Seq.map (fun ot -> ot, StableGraphLayout.computeGlobalRankingForObjectType log ot) |> Map.ofSeq

    /// <summary>
    /// Compute a global order for an event log, flattening all object types and evaluating all traces as one.
    /// Expects the log to not contain identical objects with different ID's. Use <see cref="OCEL.Types.OcelLog.MergeDuplicateObjects"/> to merge them beforehand.
    /// Based on <see href="https://doi.org/10.1111/cgf.13723">Mennens, R.J.P., Scheepens, R. and Westenberg, M.A. (2019), A stable graph layout algorithm for processes. Computer Graphics Forum, 38: 725-737</see>
    /// and <see href="https://robinmennens.github.io/Portfolio/stablegraphlayouts.html">Graph layout stability in process mining</see>
    /// </summary>
    static member ComputeGlobalOrder (log: OcelLog) =
        log |> StableGraphLayout.computeGlobalRanking |> fun (rg, sk, _) -> (rg, sk) |> GraphLayoutAlgo.computeFriendlyGlobalOrder

    /// <summary>
    /// Compute a global order, flattening for a specific object type.
    /// Expects the log to not contain identical objects with different ID's. Use <see cref="OCEL.Types.OcelLog.MergeDuplicateObjects"/> to merge them beforehand.
    /// Based on <see href="https://doi.org/10.1111/cgf.13723">Mennens, R.J.P., Scheepens, R. and Westenberg, M.A. (2019), A stable graph layout algorithm for processes. Computer Graphics Forum, 38: 725-737</see>
    /// and <see href="https://robinmennens.github.io/Portfolio/stablegraphlayouts.html">Graph layout stability in process mining</see>
    /// </summary>
    static member ComputeGlobalOrderForObjectType(log: OcelLog, objType) =
        (log, objType) ||> StableGraphLayout.computeGlobalRankingForObjectType |> fun (rg, sk, _) -> (rg, sk) |> GraphLayoutAlgo.computeFriendlyGlobalOrder

    /// <summary>
    /// Compute a global order for each object type in an event log.
    /// Expects the log to not contain identical objects with different ID's. Use <see cref="OCEL.Types.OcelLog.MergeDuplicateObjects"/> to merge them beforehand.
    /// Based on <see href="https://doi.org/10.1111/cgf.13723">Mennens, R.J.P., Scheepens, R. and Westenberg, M.A. (2019), A stable graph layout algorithm for processes. Computer Graphics Forum, 38: 725-737</see>
    /// and <see href="https://robinmennens.github.io/Portfolio/GraphLayoutAlgos.html">Graph layout stability in process mining</see>
    /// </summary>
    static member ComputeGlobalOrderForEachObjectType (log: OcelLog) =
        log |> StableGraphLayout.computeGlobalRankingForEachObjectType |> Map.map (fun _ (gr, sk, _) -> GraphLayoutAlgo.computeFriendlyGlobalOrder (gr, sk))


    (* --- Overloads for C# OCEL log type --- *)

    /// <summary>
    /// Compute a global order for an event log, flattening all object types and evaluating all traces as one.
    /// Expects the log to not contain identical objects with different ID's. Use <see cref="OCEL.Types.OcelLog.MergeDuplicateObjects"/> to merge them beforehand.
    /// Based on <see href="https://doi.org/10.1111/cgf.13723">Mennens, R.J.P., Scheepens, R. and Westenberg, M.A. (2019), A stable graph layout algorithm for processes. Computer Graphics Forum, 38: 725-737</see>
    /// and <see href="https://robinmennens.github.io/Portfolio/GraphLayoutAlgos.html">Graph layout stability in process mining</see>
    /// </summary>
    static member ComputeGlobalOrder (log: OCEL.CSharp.OcelLog) =
        log |> OCEL.CSharp.FSharpConverters.ToFSharpOcelLog |> StableGraphLayout.ComputeGlobalOrder

    /// <summary>
    /// Compute a global order, flattening for a specific object type.
    /// Expects the log to not contain identical objects with different ID's. Use <see cref="OCEL.Types.OcelLog.MergeDuplicateObjects"/> to merge them beforehand.
    /// Based on <see href="https://doi.org/10.1111/cgf.13723">Mennens, R.J.P., Scheepens, R. and Westenberg, M.A. (2019), A stable graph layout algorithm for processes. Computer Graphics Forum, 38: 725-737</see>
    /// and <see href="https://robinmennens.github.io/Portfolio/GraphLayoutAlgos.html">Graph layout stability in process mining</see>
    /// </summary>
    static member ComputeGlobalOrderForObjectType(log: OCEL.CSharp.OcelLog, objType) =
         StableGraphLayout.ComputeGlobalOrderForObjectType(log |> OCEL.CSharp.FSharpConverters.ToFSharpOcelLog, objType)

    /// <summary>
    /// Compute a global order for each object type in an event log.
    /// Expects the log to not contain identical objects with different ID's. Use <see cref="OCEL.Types.OcelLog.MergeDuplicateObjects"/> to merge them beforehand.
    /// Based on <see href="https://doi.org/10.1111/cgf.13723">Mennens, R.J.P., Scheepens, R. and Westenberg, M.A. (2019), A stable graph layout algorithm for processes. Computer Graphics Forum, 38: 725-737</see>
    /// and <see href="https://robinmennens.github.io/Portfolio/GraphLayoutAlgos.html">Graph layout stability in process mining</see>
    /// </summary>
    static member ComputeGlobalOrderForEachObjectType (log: OCEL.CSharp.OcelLog) =
        log |> OCEL.CSharp.FSharpConverters.ToFSharpOcelLog |> StableGraphLayout.ComputeGlobalOrderForEachObjectType |> Map.toSeq |> dict


    (* --- Overloads for providing methods with discovered model, in order to fix horizontal edges --- *)

    /// <summary>
    /// Compute a global order for an event log, flattening all object types and evaluating all traces as one.
    /// An already discovered model can be passed in to fix any horizontal edges that may have been created due to filtering.
    /// Expects the log to not contain identical objects with different ID's. Use <see cref="OCEL.Types.OcelLog.MergeDuplicateObjects"/> to merge them beforehand.
    /// Based on <see href="https://doi.org/10.1111/cgf.13723">Mennens, R.J.P., Scheepens, R. and Westenberg, M.A. (2019), A stable graph layout algorithm for processes. Computer Graphics Forum, 38: 725-737</see>
    /// and <see href="https://robinmennens.github.io/Portfolio/GraphLayoutAlgos.html">Graph layout stability in process mining</see>
    /// </summary>
    static member ComputeGlobalOrder (log: OcelLog, discoveredModel: DirectedGraph<Graphs.Node, Graphs.Edge>) =
        let gr, skeleton, comps = log |> StableGraphLayout.computeGlobalRanking
        let (gr, _) = GraphLayoutAlgo.fixHorizontalEdgesInGlobalRankGraphForDiscoveredModel gr comps discoveredModel
        (gr, skeleton) |> GraphLayoutAlgo.computeFriendlyGlobalOrder


    /// <summary>
    /// Compute a global order, flattening for a specific object type.
    /// An already discovered model can be passed in to fix any horizontal edges that may have been created due to filtering.
    /// Expects the log to not contain identical objects with different ID's. Use <see cref="OCEL.Types.OcelLog.MergeDuplicateObjects"/> to merge them beforehand.
    /// Based on <see href="https://doi.org/10.1111/cgf.13723">Mennens, R.J.P., Scheepens, R. and Westenberg, M.A. (2019), A stable graph layout algorithm for processes. Computer Graphics Forum, 38: 725-737</see>
    /// and <see href="https://robinmennens.github.io/Portfolio/GraphLayoutAlgos.html">Graph layout stability in process mining</see>
    /// </summary>
    static member ComputeGlobalOrderForObjectType(log: OcelLog, objType, discoveredModel: DirectedGraph<Graphs.Node, Graphs.Edge>) =
        let gr, skeleton, comps = StableGraphLayout.computeGlobalRankingForObjectType log objType
        let (gr, _) = GraphLayoutAlgo.fixHorizontalEdgesInGlobalRankGraphForDiscoveredModel gr comps discoveredModel
        (gr, skeleton) |> GraphLayoutAlgo.computeFriendlyGlobalOrder
