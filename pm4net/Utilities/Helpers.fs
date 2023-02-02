namespace pm4net.Utilities

module internal Helpers =

    /// Apply a mapping function to a nested list
    let mapNestedList mapper nestedList =
        nestedList |> List.map (fun list -> list |> List.map mapper)

    /// Return the most common value in a list, given some extractor function to extract the property (can just be id)
    let mostCommonValue extractor list =
        list
        |> List.countBy extractor
        |> List.maxBy snd
        |> fst
