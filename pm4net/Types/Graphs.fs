namespace pm4net.Types.Dfg

open pm4net.Types

type DirectedGraph<'Node, 'Edge> when 'Node : comparison = {
    Nodes: 'Node list
    Edges: ('Node * 'Node * 'Edge) list
}

type NodeStatistics = {
    Frequency: int
}

type EdgeStatistics = {
    Frequency: int
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
