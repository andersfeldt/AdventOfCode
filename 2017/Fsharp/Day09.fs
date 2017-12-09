module Day09

type State =
    {
        score: int;
        depth: int;
        isInsideGarbage: bool;
        ignoreNext: bool;
    }

let getResultA input =
    let initialState =
        {
            score = 0;
            depth = 0;
            isInsideGarbage = false;
            ignoreNext = false;
        }

    let folder state ch =
        let handleInsideGarbage state' ch' =
            match ch' with
            | '!' -> { state' with ignoreNext = true }
            | '>' -> { state' with isInsideGarbage = false }
            | _   -> state'

        let handleOutsideGarbage state' ch' =
            match ch' with
            | '<' -> { state' with isInsideGarbage = true }
            | '{' -> { state' with depth = state'.depth + 1 }
            | '}' -> { state' with score = state'.score + state'.depth; depth = state'.depth - 1 }
            | _   -> state'

        match state.isInsideGarbage, state.ignoreNext with
        | true, true  -> { state with ignoreNext = false }
        | true, false -> handleInsideGarbage state ch
        | false, _    -> handleOutsideGarbage state ch

    input
    |> Seq.fold folder initialState
    |> (fun s -> s.score)

let getResult part (input:string list) =
    match part with
    | A -> getResultA input.[0]
    | B -> failwith "Not implemented yet"
    |> string
