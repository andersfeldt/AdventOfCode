open System
open System.IO

let dayFuncs d =
    match d with
    | 1  -> Day01.getResult
    | 2  -> Day02.getResult
    | 3  -> Day03.getResult
    | 4  -> Day04.getResult
    | 5  -> Day05.getResult
    | 6  -> Day06.getResult
    | 7  -> Day07.getResult
    | 8  -> Day08.getResult
    | 9  -> Day09.getResult
    | 10 -> Day10.getResult
    | 11 -> Day11.getResult
    | 12 -> Day12.getResult
    | 13 -> Day13.getResult
    | 14 -> Day14.getResult
    | 15 -> Day15.getResult
    | 16 -> Day16.getResult
    | 17 -> Day17.getResult
    | _  -> failwith "Not implemented yet"

let run (argv:string[]) =
    let day, part =
        let tryParseInt s =
            match Int32.TryParse s with
            | true, i  -> i
            | false, _ -> failwith "Invalid day selected"

        let tryParsePart (s:string) =
            match s.ToUpper() with
            | "A" -> A
            | "B" -> B
            | _   -> failwith "Invalid part selected"

        match argv.Length with
        | 2 -> argv.[0] |> tryParseInt, argv.[1] |> tryParsePart
        | _ -> failwith "No day or part selected"

    let selectedFunc =
        match part with
        | A -> dayFuncs day A
        | B -> dayFuncs day B

    let readPuzzleInput (day:int) =
        let fileName = sprintf "Day%02i.txt" day
        let path = Path.Combine(__SOURCE_DIRECTORY__, "..", "PuzzleInputs", fileName)
        let hasContent s = not (String.IsNullOrWhiteSpace(s))

        File.ReadAllLines(path)
        |> List.ofArray
        |> List.filter hasContent

    readPuzzleInput day
    |> selectedFunc
    |> Console.WriteLine

[<EntryPoint>]
let main argv =
    try
        run argv
    with
    | Failure message                -> printfn "%s" message
    | :? FileNotFoundException as ex -> printfn "%s" ex.Message

    0
