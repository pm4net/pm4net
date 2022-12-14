namespace pm4py.Algorithms.Discovery.Ocel

open pm4net.Types

module OcelPetriNet =
    /// Discovers an object-centric Petri net (without annotation) from the given object-centric event log, using the Inductive Miner as process discovery algorithm.
    /// Reference paper: van der Aalst, Wil MP, and Alessandro Berti. "Discovering object-centric Petri nets." Fundamenta informaticae 175.1-4 (2020): 1-40.
    /// Reference implementation: https://github.com/pm4py/pm4py-core/tree/release/pm4py/algo/discovery/ocel/ocpn
    let apply (log: OCEL.Types.OcelLog) : ObjectCentricPetriNet<string> =
        let flattened_logs = log.ObjectTypes |> Seq.map (fun t -> t, pm4net.Utilities.OcelUtitilies.flatten log t) |> Map.ofSeq

        {
            Places = ([]: seq<string>) |> Set.ofSeq
            Transitions = Set.empty
            Edges = Set.empty
        }
