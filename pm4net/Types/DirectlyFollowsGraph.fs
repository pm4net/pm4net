namespace pm4net.Types

open System

type NodeInfo = {
    Frequency: int
    Namespace: string option
    Level: LogLevel option
}

type EdgeInfo = {
    Durations: TimeSpan list
}
