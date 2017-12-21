module Day15

open System

let getResultA (input:string list) =
    let parse (line:string) =
        line.Split(' ')
        |> Array.last
        |> int64

    let initial =
        input
        |> List.map parse
        |> (fun x -> x.[0], x.[1], 0)

    let factorA = 16807L
    let factorB = 48271L
    let divisor = 2147483647L

    let toTruncatedBinary (value:int64) =
        let width = 16
        let binary = Convert.ToString(value, 2).PadLeft(width, '0')
        binary.Substring(binary.Length - width, width)

    let folder (a, b, count) index =
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

let getResult part (input:string list) =
    match part with
    | A -> getResultA input
    | B -> failwith "Not implemented yet"
    |> string
