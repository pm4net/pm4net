namespace pm4net.Types

type NodeInfo = {
    Frequency: int
    Namespace: string option
    Level: LogLevel option
}

type EdgeInfo = {
    A: int
}
