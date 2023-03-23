namespace pm4net.Types.GraphLayout

open pm4net.Types.Graphs

/// Type to represent a sequence of nodes and edges, where an edge consists of two nodes
type SequenceElement<'a> =
    | Node of 'a
    | Edge of 'a * 'a

/// A directed graph that represents the global rank graph. Nodes consist of activity name and rank, and edges store their frequency.
type GlobalRankGraph = DirectedGraph<string * int, int>

/// List of all node sequences, including their discovery index
type Skeleton = (SequenceElement<string> * int) list list

/// Connected nodes within a global rank graph
type Components = Set<string> list

type Connection = {
    A: string
    B: string
    Weight: int
}

/// Types of nodes that can be found in a node sequence graph
type SequenceNode =
    | Real of Rank: int * DiscoveryIndex: int * Name: string
    | Virtual of Rank: int * DiscoveryIndex: int * Connection

/// An undirected graph that represents the node sequence graph of a given rank graph and skeleton (data structure is technically directed, but use edges as two-way connections)
type NodeSequenceGraph = DirectedGraph<SequenceNode>
type GlobalOrderNodeSequenceGraph = DirectedGraph<int * SequenceNode> // Integer indicates the X position of the node

// Types for the actual discovered graph with real X,Y positions for nodes and virtual nodes

type Position = {
    X: float32
    Y: int
}

type GraphNode =
    | ConstrainedReal of Position: Position * DiscoveryIndex: int * Name: string
    | ConstrainedVirtual of Position: Position * DiscoveryIndex: int * Connection
    | UnconstrainedVirtual of Position: Position * Connection
type DiscoveredGraph = DirectedGraph<GraphNode, int>

/// Type to represent a variation of a trace, with all its events, a sequence of nodes and edges, and the frequency of the variation
type internal Variation<'a, 'b> = {
    Events: 'a list
    Sequence: (SequenceElement<'a> * 'b) list
    Frequency: int
}

(* --- "Pretty" types to expose to the callers --- *)

/// A point in a coordinate system.
type Coordinate = {
    X: float32
    Y: float32
}

/// The size of a node.
type Size = {
    Width: int
    Height: int
}

/// A node within a directed graph that has an associated name and X,Y position (origin in top-left).
type Node = {
    Id: string
    Text: string list
    Type: string
    Position: Coordinate
    Size: Size
    Rank: int
}

/// A path between two nodes with several waypoints which the edge should go through.
type EdgePath = {
    Edge: string * string
    Waypoints: Coordinate seq
    Downwards: bool
}

/// A global order defines a collection of nodes and their position, as well as paths between nodes where non-straight edges are required.
type GraphLayout = {
    Nodes: Node seq
    Edges: Connection seq
    EdgePaths: EdgePath seq
}

// Helper types for the above

type internal NodeOrCoordinate =
    | InternalNode of Node
    | InternalCoordinate of Coordinate
