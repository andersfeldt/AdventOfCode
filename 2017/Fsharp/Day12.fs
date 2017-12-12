module Day12

open System

let getResultA (input:string list) =
    let parse (line:string) =
        line.Split("<-> ,".ToCharArray(), StringSplitOptions.RemoveEmptyEntries)
        |> Array.map int
        |> Set.ofArray

    let data = input |> List.map parse

    let result initialState =
        let folder state current =
            let isIntersecting =
                state
                |> Set.intersect current
                |> Set.isEmpty
                |> not
            match isIntersecting with
            | true  -> Set.union state current
            | false -> state
        data
        |> List.fold folder initialState

    let scanner (selectedSet, _) _ =
        (result selectedSet, Set.count selectedSet)

    let stopCondition (selectedSet, lastLength) =
        (Set.count selectedSet) = lastLength

    Seq.initInfinite id
    |> Seq.scan scanner ((Set.ofList [0]), 0)
    |> Seq.find stopCondition
    |> snd

let getResult part (input:string list) =
    match part with
    | A -> getResultA input
    | B -> failwith "Not implemented yet"
    |> string
