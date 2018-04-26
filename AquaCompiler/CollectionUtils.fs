module Aqua.CollectionUtils

open System.Collections.Generic


// wrapper for readonly array
//

type ArrayView<'T> = IReadOnlyList<'T>

module ArrayView =
    let ofArray (data: 'a array) =
        data :> ArrayView<_>

    let ofSeq data =
        (Array.ofSeq data) :> ArrayView<_>

    let length (x: ArrayView<_>) =
        x.Count

    let item i (x: ArrayView<_>) =
        x.Item(i)

    let iter f (x: ArrayView<_>) =
        for elem in x do f elem

    let map f (x: ArrayView<_>) =
        x |> Seq.map f |> ofSeq

    let mapi f (x: ArrayView<_>) =
        x |> Seq.mapi f |> ofSeq

    let reduce f (x: ArrayView<_>) =
        x |> Seq.reduce f

    let reduceBack f (x: ArrayView<_>) =
        x |> Seq.reduceBack f

    let fold f state (x: ArrayView<_>) =
        x |> Seq.fold f state

    let foldBack f state (x: ArrayView<_>) =
        x |> Seq.foldBack f state

// wrapper for readonly dictionary
//

type DictView<'K, 'V> = IReadOnlyDictionary<'K, 'V>

module DictView =
    let ofSeq (data: seq<'K * 'V>) =
        let d = Dictionary()
        for elem in data do
            d.Add(elem)
        
        d :> IReadOnlyDictionary<_, _>

    let length (x: DictView<_, _>) =
        x.Count

    let find key (x: DictView<_, _>) =
        x.Item(key)

    let tryFind key (x: DictView<_, _>) =
        match x.TryGetValue key with
        | true, x -> Some x
        | false, _ -> None

    let iter f (x: DictView<_, _>) =
        for elem in x do f elem