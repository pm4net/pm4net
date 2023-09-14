namespace pm4net.Types

open OCEL.Types
open System

type NodeInfo = {
    Frequency: int
    Namespace: string option
    Level: LogLevel option
    Attributes: Map<string, OcelValue>
    Objects: OcelObject list
}

type EdgeInfo = {
    Durations: TimeSpan seq
}
