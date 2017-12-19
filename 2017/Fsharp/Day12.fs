module Day12

open System

let parse (line:string) =
    line.Split("<-> ,".ToCharArray(), StringSplitOptions.RemoveEmptyEntries)
    |> Array.map int
    |> Set.ofArray

let getGroup data initialSet =
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
    |> Seq.scan scanner (initialSet, 0)
    |> Seq.find stopCondition

let getResultA (input:string list) =
    let data = List.map parse input

    getGroup data (Set.ofList [0])
    |> snd

let getResultB (input:string list) =
    let scanner ((state:Set<int> list), (minElement:int), (count:int)) _ =
        let group, _ = getGroup state (Set.ofList [minElement])

        let newState =
            state
            |> List.filter (fun set -> Set.intersect group set |> Set.isEmpty)

        let newMinElement =
            match newState with
            | [] -> 0
            | newState' ->
                (newState'
                |> List.map (Set.minElement)
                |> List.min)

        (newState, newMinElement, count + 1)

    let stopCondition (state, _, _) = List.isEmpty state

    let initialState = List.map parse input

    Seq.initInfinite id
    |> Seq.scan scanner (initialState, 0, 0)
    |> Seq.find stopCondition
    |> fun (_, _, i) -> i

let getResult part (input:string list) =
    match part with
    | A -> getResultA input
    | B -> getResultB input
    |> string
