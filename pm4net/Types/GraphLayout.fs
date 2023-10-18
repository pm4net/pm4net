namespace pm4net.Types.GraphLayout

open pm4net.Types

/// Indicates which nodes a virtual waypoint connects, and what weight the edge has.
type Connection = {
    A: string
    B: string
    Weight: int
}

/// Types of nodes that can be found in a node sequence graph
type SequenceNode =
    | Real of Rank: int * DiscoveryIndex: int * Name: string
    | Virtual of Rank: int * DiscoveryIndex: int * Connection
    member x.TryReal (rank: int byref, discoveryIndex: int byref, name: string byref) =
        match x with
        | Real(r, d, n) -> rank <- r; discoveryIndex <- d; name <- n; true
        | _ -> false
    member x.TryVirtual (rank: int byref, discoveryIndex: int byref, connection: Connection byref) =
        match x with
        | Virtual(r, d, c) -> rank <- r; discoveryIndex <- d; connection <- c; true
        | _ -> false

/// Type to represent a sequence of nodes and edges, where an edge consists of two nodes
type SequenceElement<'a> =
    | Node of 'a
    | Edge of 'a * 'a
    member x.TryNode (node: 'a byref) =
        match x with
        | Node(v) -> node <- v; true
        | _ -> false
    member x.TryEdge (a: 'a byref, b: 'a byref) =
        match x with
        | Edge(aN, bN) -> a <- aN; b <- bN; true
        | _ -> false

/// List of all node sequences, including their discovery index
type Skeleton = (SequenceElement<string> * int) list list

/// Connected nodes within a global rank graph
type Components = Set<string> list

/// A directed graph that represents the global rank graph. Nodes consist of activity name and rank, and edges store their frequency.
type GlobalRankGraph = DirectedGraph<string * int, int>
type GlobalRanking = {
    Graph: GlobalRankGraph
    Skeleton: Skeleton
    Components: Components
}

/// A point in a coordinate system.
type Coordinate = {
    X: float32
    Y: float32
}

/// The size of a node.
type Size = {
    Width: float32
    Height: float32
}

/// Type to represent which type of node a given node is.
type NodeType =
    | Event
    | Start of Type: string
    | End of Type: string
    member x.TryStart (objType: string byref) =
        match x with
        | Start(t) -> objType <- t; true
        | _ -> false
    member x.TryEnd (objType: string byref) =
        match x with
        | End(t) -> objType <- t; true
        | _ -> false

/// A node within a directed graph that has an associated name and X,Y position (origin in top-left).
type Node<'a> = {
    Id: string
    Text: string seq
    NodeType: NodeType
    Position: Coordinate
    Size: Size
    Rank: int
    Info: 'a option
}

/// Edge information for a specific object type
type EdgeTypeInfo<'a> = {
    Weight: int
    Type: string option
    Info: 'a option
}

/// Encapsulate different types of waypoint formats for the same edge
type Waypoints = {
    Coordinates: Coordinate seq
    CatmullRom: Coordinate seq
    CubicBezier: (Coordinate * Coordinate * Coordinate * Coordinate) seq
    QuadraticBezier: (Coordinate * Coordinate * Coordinate) seq
}

/// An edge within a directed graph that associates two nodes and potentially some waypoints.
type Edge<'a> = {
    SourceId: string
    TargetId: string
    Waypoints: Waypoints
    Downwards: bool
    Constrained: bool
    TotalWeight: int
    TypeInfos: EdgeTypeInfo<'a> seq
}

/// A global order defines a collection of nodes and their position, as well as paths between nodes where non-straight edges are required.
type GraphLayout<'a, 'b> = {
    Nodes: Node<'a> seq
    Edges: Edge<'b> seq
}

/// A position with a floating-point value on the X axis, and a fixed rank integer.
type Position = {
    X: float32
    Y: int
}

/// A graph to store both real and virtual nodes
type DiscoveredGraph<'a> = DirectedGraph<GraphNode, Edge<'a>>
and GraphNode =
    | ConstrainedReal of Position: Position * DiscoveryIndex: int * Name: string
    | ConstrainedVirtual of Position: Position * DiscoveryIndex: int * Connection
    | UnconstrainedVirtual of Position: Position * Connection
    member x.TryConstrainedReal (position: Position byref, discoveryIndex: int byref, name: string byref) =
        match x with
        | ConstrainedReal(pos, disc, n) -> position <- pos; discoveryIndex <- disc; name <- n; true
        | _ -> false
    member x.TryConstrainedVirtual (position: Position byref, discoveryIndex: int byref, connection: Connection byref) =
        match x with
        | ConstrainedVirtual(pos, disc, conn) -> position <- pos; discoveryIndex <- disc; connection <- conn; true
        | _ -> false
    member x.TryUnconstrainedVirtual (position: Position byref, connection: Connection byref) =
        match x with
        | UnconstrainedVirtual(pos, conn) -> position <- pos; connection <- conn; true
        | _ -> false
