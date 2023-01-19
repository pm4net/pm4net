namespace pm4net.Types.ProcessTree

type ProcessTree<'a> =
    | Activity of 'a
    | SilentActivity
    | Sequential of ProcessTree<'a> list
    | Exclusive of ProcessTree<'a> list
    | Parallel of ProcessTree<'a> list
    | RedoLoop of ProcessTree<'a> list
