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

/// An implementation of the "stable graph layout algorithm for processes" introduced by Mennens 2019 (https://doi.org/10.1111/cgf.13723), implemented by Johannes Mols.
/// Any OCEL log object passed in is expected to not contain identical objects with different ID's. Use <see cref="OCEL.Types.OcelLog.MergeDuplicateObjects"/> to merge them beforehand.
[<AbstractClass; Sealed>]
type StableGraphLayout private() =

    static member internal toCSharpFriendly (rg: GlobalRankGraph, sk: Skeleton, comp: Components) =
        rg, sk |> List.map Seq.ofList |> seq, comp |> List.map Set.toSeq |> seq

    /// Extract the unique variations in the flattened log and order them by importance, with an optional object type that is added as a start and end to all variations
    static member internal variationsInLog objType log =

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
                let startEvent = { Activity = Constants.objectTypeStartNode + objType; Timestamp = (events |> List.head |> fun e -> e.Timestamp.AddTicks(-1)); OMap = []; VMap = Map.empty }
                let endEvent = { Activity = Constants.objectTypeEndNode + objType; Timestamp = (events |> List.last |> fun e -> e.Timestamp.AddTicks(1)); OMap = []; VMap = Map.empty }
                startEvent :: events @ [endEvent]
            | None -> events)
        |> List.countBy (fun t -> t |> List.map (fun e -> e.Activity)) // Extract only activity name and count the occurrences of each variant/path
        |> List.map (fun (t, cnt) -> { Events = t; Frequency = cnt; Sequence = (cnt, t) ||> simpleSequence })

    /// Compute a global ranking based on a rank graph and skeleton
    static member internal computeGlobalRanking rankGraph skeleton =
        let nodeSequenceGraph = (rankGraph, skeleton) ||> GraphLayoutAlgo.computeNodeSequenceGraph
        (rankGraph, nodeSequenceGraph) ||> GraphLayoutAlgo.computeGlobalOrder


    /// Compute the global rank graph (RG) from an event log, merging the flattened logs of all object types together to get a total view.
    static member ComputeRankGraph (log: OcelLog) : GlobalRankGraph * Skeleton * Components  =
        log.ObjectTypes
        |> Set.toList
        |> List.map (fun t -> OcelHelpers.Flatten log t |> StableGraphLayout.variationsInLog (Some t))
        |> List.concat
        |> List.sortByDescending GraphLayoutAlgo.importanceSort
        |> GraphLayoutAlgo.computeGlobalRankingAndSkeletonForVariations

    /// Compute the global rank graph (RG) from an event log, merging the flattened logs of all object types together to get a total view.
    static member ComputeRankGraph (log: OCEL.CSharp.OcelLog) =
        log
        |> OCEL.CSharp.FSharpConverters.ToFSharpOcelLog
        |> StableGraphLayout.ComputeRankGraph
        |> StableGraphLayout.toCSharpFriendly


    /// Compute the global rank graph (RG) from an event log, flattening for a specific object type.
    static member ComputeRankGraphForObjectType (log: OcelLog, objectType) : GlobalRankGraph * Skeleton * Components =
        OcelHelpers.Flatten log objectType
        |> StableGraphLayout.variationsInLog None
        |> List.sortByDescending GraphLayoutAlgo.importanceSort
        |> GraphLayoutAlgo.computeGlobalRankingAndSkeletonForVariations

    /// Compute the global rank graph (RG) from an event log, flattening for a specific object type.
    static member ComputeRankGraphForObjectType (log: OCEL.CSharp.OcelLog, objectType) =
        (log |> OCEL.CSharp.FSharpConverters.ToFSharpOcelLog, objectType)
        |> StableGraphLayout.ComputeRankGraphForObjectType
        |> StableGraphLayout.toCSharpFriendly


    /// Compute the global rank graph (RG) from an event log for each object type available in the log.
    static member ComputeRankGraphForEachObjectType (log: OcelLog) : Map<string, GlobalRankGraph * Skeleton * Components> =
        log.ObjectTypes
        |> Seq.map (fun ot -> ot, (log, ot) |> StableGraphLayout.ComputeRankGraphForObjectType)
        |> Map.ofSeq

    /// Compute the global rank graph (RG) from an event log for each object type available in the log.
    static member ComputeRankGraphForEachObjectType (log: OCEL.CSharp.OcelLog) =
        log
        |> OCEL.CSharp.FSharpConverters.ToFSharpOcelLog
        |> StableGraphLayout.ComputeRankGraphForEachObjectType
        |> Map.map (fun _ v -> v |> StableGraphLayout.toCSharpFriendly)
        |> Map.toSeq
        |> dict

    (* --- GLOBAL ORDER FOR ALL OBJECT TYPES --- *)

    /// Compute a global order for a global rank graph by the means of a discovered model that is a subgraph of the global rank graph.
    /// Edges that are identical in its origin and destination can be merged together in order to avoid multiple edges in the resulting graph.
    static member ComputeGlobalOrder (rankGraph, skeleton, components, discoveredModel, mergeEdges, maxCharsPerLine, nodesep, ranksep, edgesep) =
        let (rankGraph, _) = (rankGraph, components, discoveredModel) |||> GraphLayoutAlgo.fixHorizontalEdgesInGlobalRankGraphForDiscoveredModel
        let globalOrder = (rankGraph, skeleton) ||> StableGraphLayout.computeGlobalRanking
        let discoveredGraph = (globalOrder, skeleton, discoveredModel) |||> GraphLayoutAlgo.constructDiscoveredGraph mergeEdges
        discoveredGraph |> GraphLayoutAlgo.computeNodePositions maxCharsPerLine nodesep ranksep edgesep

    /// Compute a global order for a global rank graph by the means of a discovered model that is a subgraph of the global rank graph.
    /// Edges that are identical in its origin and destination can be merged together in order to avoid multiple edges in the resulting graph.
    static member ComputeGlobalOrder (
        rankGraph: GlobalRankGraph,
        skeleton: (SequenceElement<string> * int) seq seq,
        components: string seq seq,
        discoveredModel: DirectedGraph<Graphs.Node, Graphs.Edge>,
        mergeEdges,
        maxCharsPerLine,
        nodesep,
        ranksep,
        edgesep) =
            StableGraphLayout.ComputeGlobalOrder(
                rankGraph,
                skeleton |> Seq.map List.ofSeq |> List.ofSeq,
                components |> Seq.map Set.ofSeq |> List.ofSeq,
                discoveredModel,
                mergeEdges,
                maxCharsPerLine,
                nodesep,
                ranksep,
                edgesep)
