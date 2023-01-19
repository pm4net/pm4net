namespace pm4net.Types.PetriNets

type ObjectCentricPetriNet<'a when 'a : comparison> = {
    Places: Set<'a>
    Transitions: Set<'a>
    Edges: Set<'a * 'a>
}
