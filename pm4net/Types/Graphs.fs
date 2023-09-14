namespace pm4net.Types

/// Directed graph with no edge information.
type DirectedGraph<'Node> when 'Node : comparison = {
    Nodes: 'Node list
    Edges: ('Node * 'Node) list
}

/// Directed graph with edge information.
type DirectedGraph<'Node, 'Edge> when 'Node : comparison = {
    Nodes: 'Node list
    Edges: ('Node * 'Node * 'Edge) list
}
