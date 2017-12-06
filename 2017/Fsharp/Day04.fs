module Day04

open System

let getResultA input =
    let isValidPassPhrase (line:string) =
        let words = line.Split(' ') |> List.ofArray
        let uniqueWordCount =
            words
            |> List.distinct
            |> List.length
        uniqueWordCount = List.length words

    input
    |> List.filter isValidPassPhrase
    |> List.length

let getResultB input =
    let isValidPassPhrase (line:string) =
        let words =
            line.Split(' ')
            |> List.ofArray
            |> List.map (fun s -> s.ToCharArray() |> Array.sort |> String.Concat)

        let uniqueWordCount =
            words
            |> List.distinct
            |> List.length
        uniqueWordCount = List.length words

    input
    |> List.filter isValidPassPhrase
    |> List.length

let getResult part (input:string list) =
    match part with
    | A -> getResultA input
    | B -> getResultB input
    |> string
