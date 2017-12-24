module Day15

open System

let parse (line:string) =
    line.Split(' ')
    |> Array.last
    |> int64

let factorA = 16807L
let factorB = 48271L
let divisor = 2147483647L

let toTruncatedBinary (value:int64) =
    let width = 16
    let binary = Convert.ToString(value, 2).PadLeft(width, '0')
    binary.Substring(binary.Length - width, width)

let getResultA (input:string list) =
    let initial =
        input
        |> List.map parse
        |> (fun x -> x.[0], x.[1], 0)

    let folder (a, b, count) _ =
        let newA = (a * factorA) % divisor
        let newB = (b * factorB) % divisor
        let newCount =
            match (toTruncatedBinary newA, toTruncatedBinary newB) with
            | binaryA, binaryB when binaryA = binaryB -> count + 1
            | _                                       -> count

        (newA, newB, newCount)

    Seq.initInfinite id
    |> Seq.take 40000000
    |> Seq.fold folder initial
    |> (fun (_, _, x) -> x)

let getResultB (input:string list) =
    let initialA, initialB =
        input
        |> List.map parse
        |> (fun x -> x.[0], x.[1])

    let folder factor value _ = (value * factor) % divisor
    let filter x y = y % x = 0L

    let generatedValuesA =
        Seq.initInfinite id
        |> Seq.scan (folder factorA) initialA
        |> Seq.filter (filter 4L)

    let generatedValuesB =
        Seq.initInfinite id
        |> Seq.scan (folder factorB) initialB
        |> Seq.filter (filter 8L)

    let judger (a, b) =
        match (toTruncatedBinary a, toTruncatedBinary b) with
        | binaryA, binaryB when binaryA = binaryB -> 1
        | _                                       -> 0

    Seq.zip generatedValuesA generatedValuesB
    |> Seq.take 5000000
    |> Seq.sumBy judger

let getResult part (input:string list) =
    match part with
    | A -> getResultA input
    | B -> getResultB input
    |> string
