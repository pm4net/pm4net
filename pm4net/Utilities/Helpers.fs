namespace pm4net.Utilities

open System
open System.Drawing
open pm4net.Types

module internal Helpers =
    
    /// Apply a mapping function to a nested list
    let mapNestedList mapper nestedList =
        nestedList |> List.map (fun list -> list |> List.map mapper)

    /// Apply a mapping function to a nested sequence
    let mapNestedSeq mapper nestedSeq =
        nestedSeq |> Seq.map (fun seq -> seq |> Seq.map mapper)

    /// Return the most common value in a list, given some extractor function to extract the property (can just be id)
    let mostCommonValue extractor list =
        list
        |> List.countBy extractor
        |> List.maxBy snd
        |> fst

    /// Generate a random color
    let randomColor () =
        let rnd = new Random()
        Color.FromArgb(rnd.Next 255, rnd.Next 255, rnd.Next 255)

    // Assign random colors to each object type to use them for edge colors
    let typeColors nodes =
        nodes
        |> List.choose (fun n -> match n with | StartNode n -> Some n | _ -> None)
        |> List.map (fun obj -> obj, randomColor())
        |> Map.ofList

    /// Find the maximum frequency of edges for all object types
    let typeMaxFrequencies edges =
        edges
        |> List.map (fun (_, _, e) -> e)
        |> List.groupBy (fun e -> e.Type)
        |> List.map (fun (k, v) -> k, v |> List.maxBy (fun e -> e.Weight) |> fun e -> e.Weight)
        |> Map.ofList

    /// Scale a value down to a given range (from https://stackoverflow.com/a/31687097/2102106)
    let scaleToRange (min: float32) max observedMin observedMax value =
        if observedMax - observedMin = 0f then
            1f
        else
            (max - min) * (value - observedMin) / (observedMax - observedMin) + min

    /// Get a unique name for a node
    let nodeName = function
        | EventNode n -> n.Name
        | StartNode n -> $"{nameof(StartNode)} {n}"
        | EndNode n -> $"{nameof(EndNode)} {n}"

    /// Get the display text for a ndoe
    let nodeText = function
        | EventNode n -> n.Name
        | StartNode n
        | EndNode n -> n

    // Get list of unique fully-qualified namespaces
    let namespaceList nodes =
        nodes
        |> List.choose (fun n -> match n with | EventNode n -> Some n | _ -> None)
        |> List.map (fun n ->
            match n.Info with
            | Some info ->
                match info.Namespace with
                | Some ns -> ns
                | _ -> String.Empty
            | _ -> String.Empty)
        |> List.distinct
