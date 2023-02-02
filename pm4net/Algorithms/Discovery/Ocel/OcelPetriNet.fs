namespace pm4net.Algorithms.Discovery.Ocel

open OCEL.Types
open pm4net.Types.PetriNets

module OcelPetriNet =

    /// Discovers an object-centric Petri net (without annotation) from the given object-centric event log, using the Inductive Miner as process discovery algorithm.
    /// Reference paper: van der Aalst, Wil MP, and Alessandro Berti. "Discovering object-centric Petri nets."
    let Discover (log: OcelLog) : ObjectCentricPetriNet<string> =
        let dfgByTypes = OcelDfg.Discover(0, 0, 0, [], log)

        {
            Places = ([]: seq<string>) |> Set.ofSeq
            Transitions = Set.empty
            Edges = Set.empty
        }
