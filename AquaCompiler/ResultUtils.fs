module Aqua.ResultUtils

module Result =
    let rearrange results =
        let folder (okList, errorList) elem =
            match elem with
            | Ok x -> (x::okList, errorList)
            | Error x -> (okList, x::errorList)

        List.fold folder ([], []) results