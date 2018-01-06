module Day22

type Direction =
    | Up
    | Down
    | Left
    | Right

type NodeStatus =
    | Clean
    | Weakened
    | Infected
    | Flagged

let parse (input:string list) (cleanValue, infectedValue) =
    let center = (List.length input - 1) / 2
    let parseLine rowIndex (row:string) =
        row
        |> Seq.mapi (fun columnIndex ch -> (rowIndex, columnIndex - center), if ch = '#' then infectedValue else cleanValue)
        |> Seq.toList

    input
    |> List.mapi (fun rowIndex row -> parseLine (rowIndex - center) row)
    |> List.concat
    |> Map.ofList

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

let getResultA (input:string list) =
    let folder ((map:Map<(int*int),bool>), currentPosition, currentDirection, count) _ =
        let newDirection, newMap, newCount =
            if Map.find currentPosition map
                then turnRight currentDirection, Map.add currentPosition false map, count
                else turnLeft currentDirection,  Map.add currentPosition true map,  count + 1
        let newPosition = move currentPosition newDirection
        let newMap2 =
            if Map.containsKey newPosition newMap
                then newMap
                else Map.add newPosition false newMap

        newMap2, newPosition, newDirection, newCount

    let initialMap = parse input (false, true)

    Seq.initInfinite id
    |> Seq.take 10000
    |> Seq.fold folder (initialMap, (0, 0), Up, 0)
    |> (fun (_, _, _, i) -> i)

let getResultB (input:string list) =
    let folder ((map:Map<(int*int),NodeStatus>), currentPosition, currentDirection, count) _ =
        let currentStatus = map.[currentPosition]
        let newDirection, newMap, newCount =
            match currentStatus with
            | Clean    -> turnLeft currentDirection,               Map.add currentPosition Weakened map, count
            | Weakened -> currentDirection,                        Map.add currentPosition Infected map, count + 1
            | Infected -> turnRight currentDirection,              Map.add currentPosition Flagged map,  count
            | Flagged  -> (turnLeft >> turnLeft) currentDirection, Map.add currentPosition Clean map,    count
        let newPosition = move currentPosition newDirection
        let newMap2 =
            if Map.containsKey newPosition newMap
                then newMap
                else Map.add newPosition Clean newMap

        newMap2, newPosition, newDirection, newCount

    let initialMap = parse input (Clean, Infected)

    Seq.initInfinite id
    |> Seq.take 10000000
    |> Seq.fold folder (initialMap, (0, 0), Up, 0)
    |> (fun (_, _, _, i) -> i)

let getResult part (input:string list) =
    match part with
    | A -> getResultA input
    | B -> getResultB input
    |> string
