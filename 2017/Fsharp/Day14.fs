module Day14

open System

let rowHash input row = sprintf "%s-%i" input row

let hexToBinary (hex:string) =
    let hexToInt c = Convert.ToInt32(c |> string, 16)
    let intToBinary (i:int) = Convert.ToString(i, 2).PadLeft(4, '0')

    hex
    |> Seq.map (hexToInt >> intToBinary)
    |> String.Concat

let getResultA (input:string) =
    let countUsed (row:string) =
        row
        |> Seq.filter (fun c -> c = '1')
        |> Seq.length

    [0..127]
    |> List.sumBy ((rowHash input) >> (Day10.getResultB) >> hexToBinary >> countUsed)

let getResultB (input:string) =
    let findNeighbors set (x, y) =
        [(x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1)]
        |> Set.ofList
        |> Set.intersect set

    let folder (set, count) _ =
        let rec innerFolder (set, selected, _) _ =
            let newSelected =
                selected
                |> Seq.collect (findNeighbors set)
                |> Set.ofSeq

            if Set.isEmpty newSelected
                then (set, newSelected, true)
                else innerFolder (Set.difference set newSelected, newSelected, false) 0

        let current =
            set
            |> Set.toSeq
            |> Seq.head

        let selected =
            findNeighbors set current
            |> Set.add current

        let newSet =
            Seq.initInfinite id
            |> Seq.scan innerFolder (Set.difference set selected, selected, false)
            |> Seq.find (fun (_, _, x) -> x)
            |> (fun (x, _, _) -> x)

        newSet, count + 1

    let initialSet =
        [0..127]
        |> List.map ((rowHash input) >> (Day10.getResultB) >> hexToBinary)
        |> List.mapi (fun rowIndex row -> row |> Seq.mapi (fun columnIndex ch -> (columnIndex, rowIndex), ch = '1') |> Seq.toList)
        |> List.concat
        |> List.filter (fun (_, value) -> value)
        |> List.map fst
        |> Set.ofList

    Seq.initInfinite id
    |> Seq.scan folder (initialSet, 0)
    |> Seq.find (fst >> Set.isEmpty)
    |> snd

let getResult part (input:string list) =
    match part with
    | A -> getResultA input.[0]
    | B -> getResultB input.[0]
    |> string
