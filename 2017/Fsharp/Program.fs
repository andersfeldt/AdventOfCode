open System
open System.IO

let dayFuncs d =
    match d with
    | 1 -> Day01.getResult
    | 2 -> Day02.getResult
    | _ -> failwith "Not implemented yet"

let run (argv:string[]) =
    let tryParseInt s =
        match Int32.TryParse s with
        | true, i  -> i
        | false, _ -> failwith "Invalid day selected"

    let tryParsePart (s:string) =
        match s.ToUpper() with
        | "A" -> A
        | "B" -> B
        | _   -> failwith "Invalid part selected"

    let day, part =
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
        File.ReadAllLines(path) |> Array.filter Common.hasContent

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
