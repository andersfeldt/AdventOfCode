module Day21

open System

let getResultA (input:string list) =
    let getSize (count:int) = count |> float |> sqrt |> int

    let parseRules input =
        let parse (code:string) =
            let pattern, replacement =
                code.Replace("/", "").Split(" =>".ToCharArray(), StringSplitOptions.RemoveEmptyEntries)
                |> (fun x -> x.[0], x.[1])
            let size = getSize pattern.Length
            let rotate str rotateFunc =
                str
                |> Seq.chunkBySize size
                |> Seq.toList
                |> List.map List.ofArray
                |> rotateFunc
                |> List.concat
                |> List.map string
                |> String.Concat
            let allRotations str =
                [id; List.rev; List.map List.rev; List.rev >> (List.map List.rev)]
                |> List.map (rotate str)
            let transpose (str:string) =
                [for a in [0 .. (size - 1)] do
                    for b in [0 .. size .. (size * size - 1)] do
                        yield string str.[a + b]]
                |> String.Concat

            [pattern; transpose pattern]
            |> List.collect allRotations
            |> List.map (fun s -> s, replacement)

        input
        |> List.collect parse
        |> Map.ofList

    let stringToSquare (str:string) xSubSquare ySubSquare =
        let size = getSize str.Length
        let xOffset = xSubSquare * size
        let yOffset = ySubSquare * size
        str
        |> Seq.mapi (fun i ch -> ((i / size) + xOffset, (i % size) + yOffset), ch)
        |> Seq.toList

    let squareMapToString (map:Map<int*int,char>) size xSubSquare ySubSquare =
        let start subSquare = subSquare * size
        let stop subSquare = (subSquare + 1) * size - 1
        [for x in [(start xSubSquare) .. (stop xSubSquare)] do
            for y in [(start ySubSquare) .. (stop ySubSquare)] do
                yield map.[x, y]]
        |> String.Concat

    let rules = parseRules input

    let folder (map:Map<int*int,char>) _ =
        let count = Map.count map
        let size = getSize count
        let twoOrThree, numberOfSubSquares =
            match ((size % 2, size / 2), (size % 3, size / 3)) with
            | ((0, i), _) -> 2, i
            | (_, (0, i)) -> 3, i
            | _           -> failwith "Not supported"

        [for x in [0 .. (numberOfSubSquares - 1)] do
            for y in [0 .. (numberOfSubSquares - 1)] do
                yield (x, y, (squareMapToString map twoOrThree x y))]
        |> List.collect (fun (x, y, str) -> stringToSquare rules.[str] x y)
        |> Map.ofList

    let numberOfIterations = 5
    let initialString = ".#...####"
    let initialMap =
        stringToSquare initialString 0 0
        |> Map.ofList

    [1..numberOfIterations]
    |> List.fold folder initialMap
    |> Map.toList
    |> List.map snd
    |> List.filter (fun ch -> ch = '#')
    |> List.length

let getResult part (input:string list) =
    match part with
    | A -> getResultA input
    | B -> failwith "Not implemented yet"
    |> string
