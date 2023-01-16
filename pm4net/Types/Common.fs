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
