namespace pm4net.Algorithms.Discovery.Ocel

open pm4net.Types
open pm4net.Utilities
open pm4net.Utilities.HelperFunctions

module OcelPetriNet =

    /// Discovers an object-centric Petri net (without annotation) from the given object-centric event log, using the Inductive Miner as process discovery algorithm.
    /// Reference paper: van der Aalst, Wil MP, and Alessandro Berti. "Discovering object-centric Petri nets." Fundamenta informaticae 175.1-4 (2020): 1-40.
    /// discoverence implementation: https://github.com/pm4py/pm4py-core/tree/release/pm4py/algo/discovery/ocel/ocpn
    let discover (log: OCEL.Types.OcelLog) : ObjectCentricPetriNet<string> =
        let dfgByTypes = OcelDirectlyFollowsGraph.discoverFromLog log 0 0 0

        {
            Places = ([]: seq<string>) |> Set.ofSeq
            Transitions = Set.empty
            Edges = Set.empty
        }
