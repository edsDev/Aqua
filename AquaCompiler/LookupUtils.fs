module Aqua.LookupUtils

open System.Collections.Generic

type Lookup<'K, 'V> = IDictionary<'K, 'V>

module Lookup =
    let create (data: seq<'K * 'V>) =
        dict data

    let length (src: Lookup<_, _>) =
        src.Count

    let find key (src: Lookup<_, _>) =
        src.Item key

    let tryFind key (src: Lookup<_, _>) =
        match src.TryGetValue key with
        | true, x -> Some x
        | false, _ -> None
    
    let iter f (src: Lookup<_, _>) =
        for x in src do f x