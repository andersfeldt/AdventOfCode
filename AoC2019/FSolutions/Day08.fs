module FSolutions.Day08

open System

let input = PuzzleInput.singleLine 8

// Part A

let imageData =
  let parse (ch: char) = ch |> string |> int
  input
  |> Seq.map parse
  |> Seq.toList

let pixelsPerLayer = 25 * 6

let countOccurrences (x: 'a) (xs: 'a list) =
  xs
  |> List.filter (fun x' -> x' = x)
  |> List.length

let selectedLayer =
  imageData
  |> List.chunkBySize pixelsPerLayer
  |> List.map (fun layer -> layer, layer |> countOccurrences 0)
  |> List.sortBy snd
  |> List.map fst
  |> List.head

let resultPartA =
  let numberOfOnes = selectedLayer |> countOccurrences 1
  let numberOfTwos = selectedLayer |> countOccurrences 2
  numberOfOnes * numberOfTwos

// Part B

let combinedLayer =
  let layers =
    imageData
    |> List.chunkBySize pixelsPerLayer
  let isTransparent i =
    i = 2
  let resultingPixel pixels =
    pixels
    |> List.skipWhile isTransparent
    |> List.head
  let cross index =
    layers
    |> List.map (fun layer -> layer.[index])
  List.init pixelsPerLayer id
  |> List.map (cross >> resultingPixel)

let asciiArt layer =
  let width = 25
  layer
  |> List.chunkBySize width
  |> List.map (fun xs -> String.Join(' ', xs).Replace(" ", "").Replace("0", " ").Replace("1", "*"))
  |> (fun xs -> String.Join('\n', xs))
  |> sprintf "%s"

let resultPartB =
  asciiArt combinedLayer
