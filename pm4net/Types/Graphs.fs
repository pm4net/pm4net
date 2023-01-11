namespace pm4net.Types

type DirectedGraph<'Node, 'Edge> when 'Node : comparison = {
    Nodes: Map<'Node, 'Edge>
    Edges: Map<'Node * 'Node, 'Edge>
}

type DiGraph<'Node, 'Edge> when 'Node : comparison = {
    Nodes: 'Node list
    Edges: ('Node * 'Node * 'Edge) list
}
