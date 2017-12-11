module Day10

type State =
    {
        numbers: int list;
        currentPosition: int;
        skipSize: int;
    }

let getResultA (input:string) =
    let reverseSubListNonWrapped startIndex length list =
        list
        |> List.splitAt startIndex
        |> (fun (part1, part2) -> part1, (part2 |> List.splitAt length))
        |> (fun (part1, (part2, part3)) -> List.concat [ part1; (List.rev part2); part3 ])

    let reverseSubListWrapped startIndex length list =
        let swapSubLists index list' =
            list'
            |> List.splitAt index
            |> (fun (part1, part2) -> List.concat [ part2; part1 ])

        list
        |> swapSubLists startIndex
        |> (fun wrapped -> reverseSubListNonWrapped 0 length wrapped)
        |> (fun reversed -> swapSubLists (List.length list - startIndex) reversed)

    let reverseSubList startIndex length list =
        match length, (startIndex + length > List.length list) with
        | 0, _     -> list
        | 1, _     -> list
        | _, true  -> reverseSubListWrapped startIndex length list
        | _, false -> reverseSubListNonWrapped startIndex length list

    let folder state length =
        {
            state with
                numbers = reverseSubList state.currentPosition length state.numbers;
                currentPosition = (state.currentPosition + length + state.skipSize) % List.length state.numbers;
                skipSize = state.skipSize + 1;
        }

    let initialState =
        {
            numbers = [0..255];
            currentPosition = 0;
            skipSize = 0;
        }

    input.Split(',')
    |> List.ofArray
    |> List.map int
    |> List.fold folder initialState
    |> (fun state -> List.take 2 state.numbers)
    |> List.reduce (*)

let getResult part (input:string list) =
    match part with
    | A -> getResultA input.[0]
    | B -> failwith "Not implemented yet"
    |> string
