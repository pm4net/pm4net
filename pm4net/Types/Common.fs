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
        member _.FromString = function
            | "Verbose" | "Trace" -> Verbose
            | "Debug" -> Debug
            | "Information" -> Information
            | "Warning" -> Warning
            | "Error" -> Error
            | "Fatal" -> Fatal
            | _ -> Unknown
