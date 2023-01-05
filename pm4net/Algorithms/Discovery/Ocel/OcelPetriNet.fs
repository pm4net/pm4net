namespace pm4py.Algorithms.Discovery.Ocel

open pm4net.Types
open pm4net.Utilities

module OcelPetriNet =
    /// Discovers an object-centric Petri net (without annotation) from the given object-centric event log, using the Inductive Miner as process discovery algorithm.
    /// Reference paper: van der Aalst, Wil MP, and Alessandro Berti. "Discovering object-centric Petri nets." Fundamenta informaticae 175.1-4 (2020): 1-40.
    /// Reference implementation: https://github.com/pm4py/pm4py-core/tree/release/pm4py/algo/discovery/ocel/ocpn
    let apply (log: OCEL.Types.OcelLog) : ObjectCentricPetriNet<string> =
        let flattenedByTypes = log.ObjectTypes |> Seq.map (fun t -> t, OcelUtitilies.flatten log t) |> Map.ofSeq
        let tracesByTypes = flattenedByTypes |> Map.map (fun _ v -> OcelUtitilies.tracesOfFlattenedLog v)
        let relationsByTypes = tracesByTypes |> Map.map (fun _ v -> OcelUtitilies.relationsFootprint v)
        let dfgByTypes = tracesByTypes |> Map.map (fun _ v -> OcelUtitilies.directlyFollowsGraph v 5 5 5)

        {
            Places = ([]: seq<string>) |> Set.ofSeq
            Transitions = Set.empty
            Edges = Set.empty
        }
