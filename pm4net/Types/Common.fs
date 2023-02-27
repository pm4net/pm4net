namespace pm4net.Types

type LogLevel =
    | Verbose
    | Debug
    | Information
    | Warning
    | Error
    | Fatal
    | Unknown
    with
        static member FromString = function
            | "Verbose" | "Trace" -> Verbose
            | "Debug" -> Debug
            | "Information" -> Information
            | "Warning" -> Warning
            | "Error" -> Error
            | "Fatal" -> Fatal
            | _ -> Unknown

module Constants =

    [<Literal>]
    let ``namespace`` = "pm4net_Namespace"
    
    [<Literal>]
    let sourceContext = "SourceContext"

    [<Literal>]
    let level = "pm4net_Level"

    [<Literal>]
    let objectTypeStartNode = "pm4net_Start-"

    [<Literal>]
    let objectTypeEndNode = "pm4net_End-"
