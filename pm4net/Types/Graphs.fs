namespace pm4net.Types

type DirectlyFollowsGraph<'Node, 'Edge> when 'Node : comparison = {
    Nodes: Map<'Node, 'Edge>
    Edges: Map<'Node * 'Node, 'Edge>
}
