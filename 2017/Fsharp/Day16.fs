module Day16

open System
open System.Text.RegularExpressions

type DanceMove =
    | Spin     of int
    | Exchange of int * int
    | Partner  of string * string

let parseInstructions (input:string) =
    let (|ActivePatternSpin|_|) code =
        let m = Regex.Match(code, @"s(\d+)")
        match m.Success with
        | true  -> Some (Spin (m.Groups.[1].Value |> int))
        | false -> None

    let (|ActivePatternExchange|_|) code =
        let m = Regex.Match(code, @"x(\d+)/(\d+)")
        match m.Success with
        | true  -> Some (Exchange (m.Groups.[1].Value |> int, m.Groups.[2].Value |> int))
        | false -> None

    let (|ActivePatternPartner|_|) code =
        let m = Regex.Match(code, @"p([a-z])/([a-z])")
        match m.Success with
        | true  -> Some (Partner (m.Groups.[1].Value, m.Groups.[2].Value))
        | false -> None

    let parse (code:string) =
        match code with
        | ActivePatternSpin spin         -> spin
        | ActivePatternExchange exchange -> exchange
        | ActivePatternPartner partner   -> partner
        | x                              -> failwithf "Not supported %A" x

    input.Split(',')
    |> List.ofArray
    |> List.map parse

let move state danceMove =
    let moveSpin size state =
        state
        |> List.splitAt (List.length state - size)
        |> (fun (a, b) -> List.append b a)

    let moveExchange positionA positionB state =
        state
        |> List.permute
            (function
            | i when i = positionA -> positionB
            | i when i = positionB -> positionA
            | i                    -> i)

    let movePartner nameA nameB state =
        let positionA = state |> List.findIndex (fun c -> c = nameA)
        let positionB = state |> List.findIndex (fun c -> c = nameB)
        moveExchange positionA positionB state

    match danceMove with
    | Spin size                       -> moveSpin size state
    | Exchange (positionA, positionB) -> moveExchange positionA positionB state
    | Partner (nameA, nameB)          -> movePartner nameA nameB state

let dance initialState instructions =
    instructions
    |> List.fold move initialState

let initialState =
    [0..15]
    |> List.map ((fun i -> (('a' |> int) + i) |> char |> string))

let getResultA (input:string) =
    parseInstructions input
    |> dance initialState
    |> String.Concat

let getResultB (input:string) =
    let instructions = parseInstructions input

    let folder (state, aggregatedResults, _) _ =
        let newState = dance state instructions
        let newStateString = newState |> String.Concat

        if aggregatedResults |> List.contains newStateString
            then (newState, aggregatedResults, true)
            else (newState, newStateString :: aggregatedResults, false)

    let aggregatedResults =
        Seq.initInfinite id
        |> Seq.scan folder (initialState, List.empty, false)
        |> Seq.find (fun (_, _, x) -> x)
        |> (fun (_, x, _) -> x)

    aggregatedResults
    |> List.rev
    |> List.item ((1000000000 % (List.length aggregatedResults)) - 1)

let getResult part (input:string list) =
    match part with
    | A -> getResultA input.[0]
    | B -> getResultB input.[0]
