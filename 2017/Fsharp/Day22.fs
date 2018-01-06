module Day22

type Direction =
    | Up
    | Down
    | Left
    | Right

let getResultA (input:string list) =
    let parse (input:string list) =
        let center = (List.length input - 1) / 2
        let parseLine rowIndex (row:string) =
            row
            |> Seq.mapi (fun columnIndex ch -> (rowIndex, columnIndex - center), ch = '#')
            |> Seq.toList

        input
        |> List.mapi (fun rowIndex row -> parseLine (rowIndex - center) row)
        |> List.concat
        |> Map.ofList

    let folder ((map:Map<(int*int),bool>), currentPosition, currentDirection, count) _ =
        let turnLeft direction =
            match direction with
            | Up    -> Left
            | Down  -> Right
            | Left  -> Down
            | Right -> Up

        let turnRight direction =
            match direction with
            | Up    -> Right
            | Down  -> Left
            | Left  -> Up
            | Right -> Down

        let move (row, column) direction =
            match direction with
            | Up    -> row - 1, column
            | Down  -> row + 1, column
            | Left  -> row, column - 1
            | Right -> row, column + 1

        let newDirection, newMap, newCount =
            if Map.find currentPosition map
                then turnRight currentDirection, Map.add currentPosition false map, count
                else turnLeft currentDirection, Map.add currentPosition true map, count + 1
        let newPosition = move currentPosition newDirection

        if Map.containsKey newPosition newMap
            then (newMap, newPosition, newDirection, newCount)
            else (Map.add newPosition false newMap, newPosition, newDirection, newCount)

    let initialMap = parse input

    Seq.initInfinite id
    |> Seq.take 10000
    |> Seq.fold folder (initialMap, (0, 0), Up, 0)
    |> (fun (_, _, _, i) -> i)

let getResult part (input:string list) =
    match part with
    | A -> getResultA input
    | B -> failwith "Not implemented yet"
    |> string
