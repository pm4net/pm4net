namespace pm4net.Tests

open System.IO
open Xunit

module Tests =

    [<Fact>]
    let ``Can discover object-centric Petri Net from sample log`` () =
        let json = File.ReadAllText("minimal.jsonocel")
        let log = OCEL.OcelJson.deserialize json
        let ocpn = pm4py.Algorithms.Discovery.Ocel.OcelPetriNet.apply log
        Assert.NotNull(ocpn)
