namespace pm4net.Types

open System
open pm4net.Types.Trees

type KeepCases =
    | ContainedInTimeframe
    | IntersectingTimeframe
    | StartedInTimeframe
    | CompletedInTimeframe
    | TrimToTimeframe

type TimeframeFilter = {
    From: DateTimeOffset
    To: DateTimeOffset
    KeepCases: KeepCases
}

type OcDfgFilter = {
    MinEvents: int
    MinOccurrences: int
    MinSuccessions: int
    Timeframe: TimeframeFilter option
    IncludedLogLevels: LogLevel list
    IncludedNamespaces: ListTree<string> option
}
