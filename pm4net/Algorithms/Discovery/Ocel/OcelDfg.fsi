namespace pm4net.Algorithms.Discovery.Ocel

open OCEL.Types
open pm4net.Types.Dfg

[<AbstractClass; Sealed>]
type OcelDfg =
    static member DiscoverForSingleType : int * int * int * string * OcelLog -> DirectedGraph<Node, Edge>
    static member DiscoverForSingleType : int * int * int * string * OCEL.CSharp.OcelLog -> DirectedGraph<Node, Edge>

    static member Discover : int * int * int * string list * OcelLog -> DirectedGraph<Node, Edge>
    static member Discover : int * int * int * string list * OCEL.CSharp.OcelLog -> DirectedGraph<Node, Edge>