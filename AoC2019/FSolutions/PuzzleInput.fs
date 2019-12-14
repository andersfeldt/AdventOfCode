module FSolutions.PuzzleInput

open System 
open System.IO

let private path day =
  let dirName = "PuzzleInputs"
  let generator (state: DirectoryInfo) =
    match state.EnumerateDirectories(dirName) |> Seq.tryHead with
    | None -> Some (state.Parent, state.Parent)
    | _ -> None
  let baseDirectory =
    Seq.unfold generator (DirectoryInfo(Environment.CurrentDirectory))
    |> Seq.rev
    |> Seq.head
  Path.Combine(baseDirectory.FullName, dirName, sprintf "Day%02i.txt" day)

let lines day =
  File.ReadAllLines(path day)
  |> List.ofArray

let singleLine day =
  lines day
  |> List.head
