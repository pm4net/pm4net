namespace pm4net.Types.Trees

type ListTree<'a> = Node of 'a * (ListTree<'a> list)
