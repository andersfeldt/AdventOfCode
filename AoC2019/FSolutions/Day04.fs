module FSolutions.Day04

let input = PuzzleInput.singleLine 4

// Part A

let min, max =
  let parsedInput =
    input.Split('-')
    |> Array.map int

  match parsedInput with
  | [| min'; max' |] -> min', max'
  | _ -> failwith "Invalid puzzle input"

let neverDecreases (pwChars: char list) : bool =
  let ordered = pwChars |> List.sort
  ordered = pwChars

let hasAdjacent (pwChars: char list) : bool =
  let equalPair =
    function
    | [ x; y ] -> x = y
    | _ -> false
  pwChars
  |> List.windowed 2
  |> List.exists equalPair

let isValidPassword (password: int) : bool =
  let pwChars =
    password
    |> string
    |> Seq.toList
  neverDecreases pwChars && hasAdjacent pwChars

let resultPartA =
  [min .. max]
  |> List.filter isValidPassword
  |> List.length

// Part B

let isValidPasswordB (password: int) : bool =
  if not <| isValidPassword password then false
  else
    password
    |> string
    |> Seq.countBy id
    |> Seq.map snd
    |> Seq.contains 2

let resultPartB =
  [min .. max]
  |> List.filter isValidPasswordB
  |> List.length
