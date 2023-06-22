namespace pm4net.Algorithms.Layout

open System.Runtime.CompilerServices
open OCEL.Types
open pm4net.Utilities

[<assembly: InternalsVisibleTo("pm4net.Tests")>]
do()

/// An implementation of the "stable graph layout algorithm for processes" introduced by Mennens 2019 (https://doi.org/10.1111/cgf.13723), implemented by Johannes Mols.
/// Any OCEL log object passed in is expected to not contain identical objects with different ID's. Use <see cref="OCEL.Types.OcelLog.MergeDuplicateObjects"/> to merge them beforehand.
[<AbstractClass; Sealed>]
type StableGraphLayout private() =

    /// Compute a global ranking for all object types in a log.
    static member ComputeGlobalRanking (log: OcelLog) =
        log |> OcelHelpers.AllTracesOfLog |> ProcessGraphLayout.Default.ComputeGlobalRanking

    /// Compute a global ranking for all object types in a log.
    static member ComputeGlobalRanking (log: OCEL.CSharp.OcelLog) =
        log |> OCEL.CSharp.FSharpConverters.ToFSharpOcelLog |> OcelHelpers.AllTracesOfLog |> ProcessGraphLayout.Default.ComputeGlobalRanking

    /// Compute a global ranking for a specific object type in a log.
    static member ComputeGlobalRankingForObjectType (log: OcelLog, objType) =
        log |> OcelHelpers.TracesForObjectType objType |> ProcessGraphLayout.Default.ComputeGlobalRanking

    /// Compute a global ranking for a specific object type in a log.
    static member ComputeGlobalRankingForObjectType (log: OCEL.CSharp.OcelLog, objType) =
        log |> OCEL.CSharp.FSharpConverters.ToFSharpOcelLog |> OcelHelpers.TracesForObjectType objType |> ProcessGraphLayout.Default.ComputeGlobalRanking

    /// Compute a graph layout for a discovered sub-graph based on a previously computed global ranking of the entire graph.
    static member ComputeGraphLayout (globalRanking, discoveredModel, mergeEdgesOfSameType, maxCharsPerLine, horizontalSep, verticalSep) =
        ProcessGraphLayout.Default.ComputeLayout globalRanking discoveredModel mergeEdgesOfSameType maxCharsPerLine horizontalSep verticalSep
