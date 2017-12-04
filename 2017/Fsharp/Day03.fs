module Day03

open System

let getResultA number =
    let getClosestOddRoot number =
        let root = Math.Sqrt(number |> float)
        let ceiling = Math.Ceiling(root) |> int
        let isOdd (x:int) = x % 2 = 1

        match (isOdd ceiling) with
        | true  -> ceiling
        | false -> ceiling + 1

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

let getResultB number =
    let instructions =
        let directions =
            let moveRight (x, y) = x + 1, y
            let moveLeft  (x, y) = x - 1, y
            let moveUp    (x, y) = x,     y - 1
            let moveDown  (x, y) = x,     y + 1
            Seq.initInfinite (fun i ->
                match i % 4 with
                | 0 -> moveRight
                | 1 -> moveUp
                | 2 -> moveLeft
                | _ -> moveDown)

        let numberOfStepsPerDirection =
            Seq.initInfinite (fun i -> (i + 2) / 2)

        let repeatDirection (direction, numberOfSteps) =
            [for _ in [1..numberOfSteps] do yield direction]

        Seq.zip directions numberOfStepsPerDirection
        |> Seq.collect repeatDirection

    let scanFunc (dictionary, position, _) direction =
        let getSurroundingSum dictionary (x, y) =
            let surroundingPositions =
                [for x' in [-1..1] do
                    for y' in [-1..1] do
                        if x' <> 0 || y' <> 0 then
                            yield (x + x', y + y')]

            let getValueInPosition position =
                match Map.tryFind position dictionary with
                | Some value -> value
                | None       -> 0

            surroundingPositions
            |> List.sumBy getValueInPosition

        let newPosition = direction position
        let surroundingSum = getSurroundingSum dictionary newPosition
        let newDictionary = Map.add newPosition surroundingSum dictionary
        (newDictionary, newPosition, surroundingSum)

    let initialPosition = (0, 0)
    let initialValue = 1
    let initialDictionary = [(initialPosition, initialValue)] |> Map.ofList
    let initialState = (initialDictionary, initialPosition, initialValue)

    let stoppingCondition number (_, _, currentValue) =
        currentValue > number

    instructions
    |> Seq.scan scanFunc initialState
    |> Seq.find (stoppingCondition number)
    |> (fun (_, _, value) -> value)
    |> string

let getResult part (input:string[]) =
    match part with
    | A -> getResultA (input.[0] |> int)
    | B -> getResultB (input.[0] |> int)
