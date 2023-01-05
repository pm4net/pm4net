namespace pm4net.Types

type ObjectCentricPetriNet<'a when 'a : comparison> = {
    Places: Set<'a>
    Transitions: Set<'a>
    Edges: Set<'a * 'a>
}
