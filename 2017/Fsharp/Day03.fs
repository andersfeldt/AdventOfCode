module Day03

open System

let isOdd (number:int) =
    number % 2 = 1

let getClosestOddRoot number =
    let root = Math.Sqrt(number |> float)
    let ceiling = Math.Ceiling(root) |> int
    match (isOdd ceiling) with
    | true  -> ceiling
    | false -> ceiling + 1

let getResultA number =
    let closestOddRoot = getClosestOddRoot number
    let stepSize = (closestOddRoot - 1) / 2
    let midPointsAlongSquareSides =
        [1..2..7]
        |> List.map (fun i -> closestOddRoot * closestOddRoot - stepSize * i)
    let shortestDistanceToMidPoint =
        midPointsAlongSquareSides
        |> List.map (fun i -> number - i |> abs)
        |> List.min

    shortestDistanceToMidPoint + stepSize
    |> string

let getResult part (input:string[]) =
    match part with
    | A -> getResultA (input.[0] |> int)
    | B -> failwith "Not implemented yet"
