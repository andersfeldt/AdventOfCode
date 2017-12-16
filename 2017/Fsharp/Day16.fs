module Day16

open System
open System.Text.RegularExpressions

type DanceMove =
    | Spin of int
    | Exchange of int * int
    | Partner of string * string

let getResultA (input:string) =
    let move danceMove state =
        let moveSpin size state =
            state
            |> List.splitAt (List.length state - size)
            |> (fun (a, b) -> List.append b a)

        let moveExchange positionA positionB state =
            let permutor index =
                match index with
                | i when i = positionA -> positionB
                | i when i = positionB -> positionA
                | i                    -> i
            state
            |> List.permute permutor

        let movePartner nameA nameB state =
            let positionA = state |> List.findIndex (fun c -> c = nameA)
            let positionB = state |> List.findIndex (fun c -> c = nameB)
            moveExchange positionA positionB state

        match danceMove with
        | Spin size                       -> moveSpin size state
        | Exchange (positionA, positionB) -> moveExchange positionA positionB state
        | Partner (nameA, nameB)          -> movePartner nameA nameB state

    let folder state code =
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

        match code with
        | ActivePatternSpin spin         -> move spin state
        | ActivePatternExchange exchange -> move exchange state
        | ActivePatternPartner partner   -> move partner state
        | _                              -> failwith "Not supported"

    let initialState =
        [0..15]
        |> List.map ((fun i -> (('a' |> int) + i) |> char |> string))

    input.Split(',')
    |> List.ofArray
    |> List.fold folder initialState
    |> String.Concat

let getResult part (input:string list) =
    match part with
    | A -> getResultA input.[0]
    | B -> failwith "Not implemented yet"
