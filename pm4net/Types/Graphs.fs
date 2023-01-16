namespace pm4net.Types

type DirectedGraph<'Node, 'Edge> when 'Node : comparison = {
    Nodes: 'Node list
    Edges: ('Node * 'Node * 'Edge) list
}

type DfgEventNode = {
    Name: string
    Frequency: int
    Level: LogLevel
    Namespace: string
}

type DfgNode =
    | StartNode of Type: string
    | EndNode of Type: string
    | EventNode of DfgEventNode

type DfgEdge = string * int
