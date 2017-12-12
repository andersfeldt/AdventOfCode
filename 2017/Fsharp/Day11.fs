module Day11

type Direction =
    | N
    | NW
    | NE
    | S
    | SW
    | SE

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

let getResultA (input:string) =
    input.Split(',')
    |> List.ofArray
    |> List.map parseDirection
    |> List.fold move (0, 0)
    |> (fun (x, y) -> x + y)

let getResultB (input:string) =
    let folder (position, maxDistance) direction =
        let newPosition = move position direction
        let getDistance (x, y) = (abs x) + (abs y)
        (newPosition, max maxDistance (getDistance newPosition))

    input.Split(',')
    |> List.ofArray
    |> List.map parseDirection
    |> List.fold folder ((0, 0), 0)
    |> snd

let getResult part (input:string list) =
    match part with
    | A -> getResultA input.[0]
    | B -> getResultB input.[0]
    |> string
