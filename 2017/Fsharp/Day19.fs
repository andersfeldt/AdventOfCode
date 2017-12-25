module Day19

type Direction =
    | Up
    | Down
    | Left
    | Right

type State =
    {
        map: Map<(int * int), char>;
        currentPosition: (int * int);
        currentDirection: Direction;
        letters: string;
        isCompleted: bool;
        stepCount: int;
    }

let initialState input =
    let parse rows =
        let parseRow rowIndex row =
            row
            |> Seq.mapi (fun columnIndex ch -> (columnIndex, rowIndex), ch)
            |> List.ofSeq

        rows
        |> List.mapi parseRow
        |> List.concat
        |> Map.ofList

    {
        map = parse input;
        currentPosition = (input.[0] |> Seq.findIndex (fun c -> c = '|')), 0;
        currentDirection = Down;
        letters = "";
        isCompleted = false;
        stepCount = 0;
    }

let move (x, y) direction =
    match direction with
    | Up    -> (x, y - 1)
    | Down  -> (x, y + 1)
    | Left  -> (x - 1, y)
    | Right -> (x + 1, y)

let isLetter ch = ch >= 'A' && ch <= 'Z'

let leftOrRight state =
    let leftCell = Map.tryFind (move state.currentPosition Left) state.map
    let rightCell = Map.tryFind (move state.currentPosition Right) state.map

    match leftCell, rightCell with
    | Some _,    None                   -> Left
    | Some '-',  Some _                 -> Left
    | Some c,    _      when isLetter c -> Left
    | None,      Some _                 -> Right
    | Some _,    Some '-'               -> Right
    | _,         Some c when isLetter c -> Right
    | _,         _                      -> failwith "Not supported"

let upOrDown state =
    let upCell = Map.tryFind (move state.currentPosition Up) state.map
    let downCell = Map.tryFind (move state.currentPosition Down) state.map

    match upCell, downCell with
    | Some _,   None                   -> Up
    | Some '|', Some _                 -> Up
    | Some c,   _      when isLetter c -> Up
    | None,     Some _                 -> Down
    | Some _,   Some '|'               -> Down
    | _,        Some c when isLetter c -> Down
    | _,        _                      -> failwith "Not supported"

let turn state =
    match state.currentDirection with
    | Up    -> { state with currentDirection = (leftOrRight state) }
    | Down  -> { state with currentDirection = (leftOrRight state) }
    | Left  -> { state with currentDirection = (upOrDown state) }
    | Right -> { state with currentDirection = (upOrDown state) }

let innerFolder state ch =
    match ch with
    | '|'               -> state
    | '-'               -> state
    | '+'               -> turn state
    | c when isLetter c -> { state with letters = state.letters + (c |> string) }
    | ' '               -> { state with isCompleted = true }
    | _                 -> failwith "Not supported"

let getResultA (input:string list) =
    let folder state _ =
        let newPosition = move state.currentPosition state.currentDirection
        match Map.tryFind newPosition state.map with
        | Some ch -> innerFolder { state with currentPosition = newPosition } ch
        | None    -> { state with isCompleted = true }

    Seq.initInfinite id
    |> Seq.scan folder (initialState input)
    |> Seq.find (fun state -> state.isCompleted)
    |> (fun state -> state.letters)

let getResultB (input:string list) =
    let folder state _ =
        let newPosition = move state.currentPosition state.currentDirection
        match Map.tryFind newPosition state.map with
        | Some ch -> innerFolder { state with currentPosition = newPosition; stepCount = state.stepCount + 1 } ch
        | None    -> { state with isCompleted = true }

    Seq.initInfinite id
    |> Seq.scan folder (initialState input)
    |> Seq.find (fun state -> state.isCompleted)
    |> (fun state -> state.stepCount |> string)

let getResult part (input:string list) =
    match part with
    | A -> getResultA input
    | B -> getResultB input
