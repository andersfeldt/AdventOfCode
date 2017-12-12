module Day11

type Direction =
    | N
    | NW
    | NE
    | S
    | SW
    | SE

let getResultA (input:string) =
    let parseDirection s =
        match s with
        | "n"  -> N
        | "nw" -> NW
        | "ne" -> NE
        | "s"  -> S
        | "sw" -> SW
        | "se" -> SE
        | _    -> failwith "Not supported"

    let move (x, y) direction =
        match direction with
        | NE -> (x + 1, y)
        | SE -> (x + 1, y - 1)
        | S  -> (x    , y - 1)
        | SW -> (x - 1, y)
        | NW -> (x - 1, y + 1)
        | N  -> (x    , y + 1)

    input.Split(',')
    |> List.ofArray
    |> List.map parseDirection
    |> List.fold move (0, 0)
    |> (fun (x, y) -> x + y)

let getResult part (input:string list) =
    match part with
    | A -> getResultA input.[0]
    | B -> failwith "Not implemented yet"
    |> string
