module Day13

open System

let parse (line:string) =
    line.Split(": ".ToCharArray(), StringSplitOptions.RemoveEmptyEntries)
    |> Array.map int
    |> (fun x -> (x.[0], x.[1]))

let isAtTop (time, range) =
    (time % (2 * range - 2)) = 0

let getResultA (input:string list) =
    input
    |> List.map parse
    |> List.filter isAtTop
    |> List.sumBy (fun (depth, range) -> depth * range)

let getResultB (input:string list) =
    let canPass delay =
        let severity =
            input
            |> List.map (parse >> (fun (depth, range) -> (depth + delay, range)))
            |> List.filter isAtTop
            |> List.sumBy (fun (depth, range) -> depth * range)
        severity = 0

    Seq.initInfinite id
    |> Seq.findIndex canPass

let getResult part (input:string list) =
    match part with
    | A -> getResultA input
    | B -> getResultB input
    |> string
