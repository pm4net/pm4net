namespace pm4net.Types

/// A list of events that happened in order, with some frequency and optionally a type.
type Trace = {
    Events: string seq
    Frequency: int
    Type: string option
}

(* --- Node definitions to be used in directed graph types --- *)

/// A node type that can be a regular event node or a start/end node for a specific type.
type Node<'a> =
    | StartNode of Type: string
    | EndNode of Type: string
    | EventNode of EventNode<'a>
    member x.TryStartNode (objType: string byref) =
        match x with
        | StartNode(t) -> objType <- t; true
        | _ -> false
    member x.TryEndNode (objType: string byref) =
        match x with
        | EndNode(t) -> objType <- t; true
        | _ -> false
    member x.TryEventNode (eventNode: EventNode<'a> byref) =
        match x with
        | EventNode(n) -> eventNode <- n; true
        | _ -> false

/// An event node contains at least the name of the event, and any other arbitrary information.
and EventNode<'a> = {
    Name: string
    Info: 'a option
}

(* --- Edge definitions --- *)

/// An edge is associated with a weight/frequency, and optionally a type, as well as any other arbitrary information.
type Edge<'a> = {
    Weight: int
    Type: string option
    Info: 'a option
}
