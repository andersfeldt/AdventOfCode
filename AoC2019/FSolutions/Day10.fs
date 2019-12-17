module FSolutions.Day10

open System
open System.IO
open System.Numerics

let input = PuzzleInput.lines 10

// Part A

type Position = int * int

let parse (input: string list) : Position list =
  let chooser (x, y, ch) =
    match ch with
    | '#' -> Some (x, y)
    | _ -> None
  let parseLine (y: int) (line: string) : Position list =
    line
    |> Seq.mapi (fun x ch -> x, y, ch)
    |> Seq.choose chooser
    |> Seq.toList
  input
  |> List.mapi parseLine
  |> List.collect id

let rec makeUniquePairs (items: 'a list) : ('a * 'a) list =
  match items with
  | [] -> []
  | x::ys ->
    seq {
      yield! ys |> List.map (fun y -> x, y)
      yield! makeUniquePairs ys
    }
    |> Seq.toList

let greatestCommonDivisor (a: int) (b: int) =
  let a' = a |> BigInteger
  let b' = b |> BigInteger
  BigInteger.GreatestCommonDivisor(a', b') |> int

let intermediaryPositions ((x1, y1): Position) ((x2, y2): Position) : Set<Position> =
  let xDiff = x2 - x1
  let yDiff = y2 - y1
  let gcd = greatestCommonDivisor xDiff yDiff
  let xStep = xDiff / gcd
  let yStep = yDiff / gcd
  [1 .. gcd - 1]
  |> List.map (fun i -> (x1 + i * xStep), (y1 + i * yStep))
  |> Set.ofList

let isFreeSight (occupiedPositions: Set<Position>) ((p1, p2): Position * Position) : bool =
  intermediaryPositions p1 p2
  |> Set.intersect occupiedPositions
  |> Set.isEmpty

let sortPairOfPositions ((p1, p2): Position * Position) : (Position * Position) =
  let toStr p =
    let x, y = p
    sprintf "%i,%i" x y
  let sorted =
    [ (p1, toStr p1); (p2, toStr p2) ]
    |> List.sortBy snd
    |> List.map fst
  sorted.[0], sorted.[1]

let asteroids = parse input
let asteroidSet = asteroids |> Set.ofList
let uniqueAsteroidPairs = makeUniquePairs asteroids

let allPairsWithFreeSight =
  uniqueAsteroidPairs
  |> List.filter (isFreeSight asteroidSet)
  |> List.collect (fun (p1, p2) -> [ (p1, p2); (p2, p1) ])

let bestPosition =
  allPairsWithFreeSight
  |> List.groupBy fst
  |> List.map (fun (p, pps) -> p, (pps |> List.length))
  |> List.sortBy snd
  |> List.rev
  |> List.head

let resultPartA =
  bestPosition |> snd
