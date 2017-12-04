module Day04

let isValidPassPhrase (line:string) =
    let words = line.Split(' ')
    let uniqueWordCount =
        words
        |> Seq.ofArray
        |> Seq.distinct
        |> Seq.length
    uniqueWordCount = Array.length words

let getResultA input =
    input
    |> Array.filter isValidPassPhrase
    |> Array.length
    |> string

let getResult part (input:string[]) =
    match part with
    | A -> getResultA input
    | B -> failwith "Not implemented yet"
