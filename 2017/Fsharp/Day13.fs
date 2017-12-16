module Day13

open System

let getResultA (input:string list) =
    let parse (line:string) =
        line.Split(": ".ToCharArray(), StringSplitOptions.RemoveEmptyEntries)
        |> Array.map int
        |> (fun x -> (x.[0], x.[1]))

    let isAtTop (time, range) =
        (time % (2 * range - 2)) = 0

    input
    |> List.map parse
    |> List.filter isAtTop
    |> List.sumBy (fun (depth, range) -> depth * range)

let getResult part (input:string list) =
    match part with
    | A -> getResultA input
    | B -> failwith "Not implemented yet"
    |> string
