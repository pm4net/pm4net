namespace pm4net.Types.GraphLayout

open pm4net.Types.Graphs

/// A directed graph that represents the global rank graph. Nodes consist of activity name and rank, and edges store their frequency.
type internal GlobalRankGraph = DirectedGraph<string * int, int>

/// Type to represent a sequence of nodes and edges, where an edge consists of two nodes
type internal SequenceElement<'a> =
    | Node of 'a
    | Edge of 'a * 'a

/// Type to represent a variation of a trace, with all its events, a sequence of nodes and edges, and the frequency of the variation
type internal Variation<'a, 'b> = {
    Events: 'a list
    Sequence: (SequenceElement<'a> * 'b) list
    Frequency: int
}

/// Types of nodes that can be found in a node sequence graph
type internal SequenceNode =
    | Real of Rank: int * DiscoveryIndex: int * Name: string
    | Virtual of Rank: int * DiscoveryIndex: int

/// An undirected graph that represents the node sequence graph of a given rank graph and skeleton (data structure is technically directed, but use edges as two-way connections)
type internal NodeSequenceGraph = DirectedGraph<SequenceNode>
type internal GlobalOrderNodeSequenceGraph = DirectedGraph<int * SequenceNode> // Integer indicates the X position of the node


(* --- Public types to expose to the callers --- *)

/// A node within a directed graph that has an associated name and X,Y position (origin in top-left).
type Node = {
    Name: string
    Coordinates: int * int
}

/// A path between two nodes with several waypoints which the edge should go through.
type EdgePath = {
    Edge: string * string
    Waypoints: (int * int) seq
}

/// A global order defines a collection of nodes and their position, as well as paths between nodes where non-straight edges are required.
type GlobalOrder = {
    Nodes: Node seq
    EdgePaths: EdgePath seq
}
