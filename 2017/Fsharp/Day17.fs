module Day17

let getResultA (input:string) =
    let insert value index list =
        list
        |> List.splitAt index
        |> (fun (a, b) -> List.concat [a; [value]; b])

    let stepSize = input |> int

    let folder (buffer, currentPosition) value =
        let insertPosition = ((currentPosition + stepSize) % (List.length buffer)) + 1
        let newBuffer = insert value insertPosition buffer
        (newBuffer, insertPosition)

    let buffer, position =
        [1..2017]
        |> List.fold folder ([0], 0)

    buffer.[position + 1]

type State =
    {
        length: int;
        currentPosition: int;
        zeroPosition: int;
        valueAfterZero: int;
    }

let getResultB (input:string) =
    let initialState =
        {
            length = 2;
            currentPosition = 1;
            zeroPosition = 0;
            valueAfterZero = 1;
        }

    let stepSize = input |> int

    let folder state value =
        let insertPosition = ((state.currentPosition + stepSize) % (state.length)) + 1
        let newZeroPosition, newValueAfterZero =
            match insertPosition with
            | i when i <= state.zeroPosition    -> state.zeroPosition + 1, state.valueAfterZero
            | i when i = state.zeroPosition + 1 -> state.zeroPosition, value
            | _                                 -> state.zeroPosition, state.valueAfterZero

        {
            length = state.length + 1;
            currentPosition = insertPosition;
            zeroPosition = newZeroPosition;
            valueAfterZero = newValueAfterZero;
        }

    let stopCondition state = state.length > 50000000

    Seq.initInfinite id
    |> Seq.skip 2
    |> Seq.scan folder initialState
    |> Seq.find stopCondition
    |> (fun s -> s.valueAfterZero)

let getResult part (input:string list) =
    match part with
    | A -> getResultA input.[0]
    | B -> getResultB input.[0]
    |> string
