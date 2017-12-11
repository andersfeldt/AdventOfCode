module Day10

open System

type State =
    {
        numbers: int list;
        currentPosition: int;
        skipSize: int;
    }

let folder state currentLength =
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

    {
        state with
            numbers = reverseSubList state.currentPosition currentLength state.numbers;
            currentPosition = (state.currentPosition + currentLength + state.skipSize) % List.length state.numbers;
            skipSize = state.skipSize + 1;
    }

let initialState =
    {
        numbers = [0..255];
        currentPosition = 0;
        skipSize = 0;
    }

let getResultA (input:string) =
    input.Split(',')
    |> List.ofArray
    |> List.map int
    |> List.fold folder initialState
    |> (fun state -> List.take 2 state.numbers)
    |> List.reduce (*)
    |> string

let getResultB (input:string) =
    let suffix = [17; 31; 73; 47; 23]
    input
    |> Seq.map (fun c -> c |> int)
    |> List.ofSeq
    |> (fun x -> List.concat [ x; suffix ])
    |> List.replicate 64
    |> List.concat
    |> List.fold folder initialState
    |> (fun state -> state.numbers)
    |> List.chunkBySize 16
    |> List.map (List.reduce (^^^) >> (sprintf "%02x"))
    |> String.Concat

let getResult part (input:string list) =
    match part with
    | A -> getResultA input.[0]
    | B -> getResultB input.[0]
