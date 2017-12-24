module Day14

open System

let getResultA (input:string) =
    let rowHash row = sprintf "%s-%i" input row

    let hexToBinary (hex:string) =
        let hexToInt c = Convert.ToInt32(c |> string, 16)
        let intToBinary (i:int) = Convert.ToString(i, 2)

        hex
        |> Seq.map (hexToInt >> intToBinary)
        |> String.Concat

    let countUsed (row:string) =
        row
        |> Seq.filter (fun c -> c = '1')
        |> Seq.length

    [0..127]
    |> List.sumBy (rowHash >> (Day10.getResultB) >> hexToBinary >> countUsed)

let getResult part (input:string list) =
    match part with
    | A -> getResultA input.[0]
    | B -> failwith "Not implemented yet"
    |> string
