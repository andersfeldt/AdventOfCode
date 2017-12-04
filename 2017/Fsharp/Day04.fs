module Day04

open System

let getResultA input =
    let isValidPassPhrase (line:string) =
        let words = line.Split(' ')
        let uniqueWordCount =
            words
            |> Seq.ofArray
            |> Seq.distinct
            |> Seq.length
        uniqueWordCount = Array.length words

    input
    |> Array.filter isValidPassPhrase
    |> Array.length
    |> string

let getResultB input =
    let isValidPassPhrase (line:string) =
        let words =
            line.Split(' ')
            |> Array.map (fun s -> Array.sort (s.ToCharArray()) |> String.Concat)

        let uniqueWordCount =
            words
            |> Seq.ofArray
            |> Seq.distinct
            |> Seq.length
        uniqueWordCount = Array.length words

    input
    |> Array.filter isValidPassPhrase
    |> Array.length
    |> string

let getResult part (input:string[]) =
    match part with
    | A -> getResultA input
    | B -> getResultB input
