namespace pm4net.Types

type DirectlyFollowsGraph = {
    Nodes: Map<string, int>
    Edges: Map<string * string, int>
}
