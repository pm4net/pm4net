namespace pm4net.Utilities

open System
open System.Drawing

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

    /// Scale a value down to a given range (from https://stackoverflow.com/a/31687097/2102106)
    let scaleToRange (min: float32) max observedMin observedMax value =
        if observedMax - observedMin = 0f then
            1f
        else
            (max - min) * (value - observedMin) / (observedMax - observedMin) + min
