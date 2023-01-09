namespace pm4net.Utilities

module HelperFunctions =

    /// Apply a mapping function to a nested list
    let mapNestedList mapper nestedList =
        nestedList |> List.map (fun list -> list |> List.map mapper)

