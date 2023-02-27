namespace pm4net.Types.Graphs

open System
open pm4net.Types

type DirectedGraph<'Node> when 'Node : comparison = {
    Nodes: 'Node list
    Edges: ('Node * 'Node) list
}

type DirectedGraph<'Node, 'Edge> when 'Node : comparison = {
    Nodes: 'Node list
    Edges: ('Node * 'Node * 'Edge) list
}

type NodeStatistics = {
    Frequency: int
} with
    static member Default = {
        Frequency = 1
    }

type EdgeStatistics = {
    Frequency: int
    Durations: TimeSpan list
} with
    static member Default = {
        Frequency = 1
        Durations = []
    }

type EventNode = {
    Name: string
    Level: LogLevel
    Namespace: string
    Statistics: NodeStatistics
}

type Node =
    | StartNode of Type: string
    | EndNode of Type: string
    | EventNode of EventNode

type Edge = {
    Type: string
    Statistics: EdgeStatistics
}
