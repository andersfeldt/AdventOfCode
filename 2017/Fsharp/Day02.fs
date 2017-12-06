module Day02

open System

let convertLineToList (s:string) =
    s.Split([|' '; '\t'|], StringSplitOptions.RemoveEmptyEntries)
    |> List.ofArray
    |> List.map int

let getResultA input =
    let getDifference line =
        let list = convertLineToList line
        (List.max list) - (List.min list)

    input
    |> List.sumBy getDifference

let getResultB input =
    let handleList numbers =
        let isEvenDivisible small large =
            match small = large with
            | true  -> false
            | false -> large % small = 0

        let sorted = List.sort numbers
        let reversed = List.rev sorted
        let existsElement arr divisor = List.exists (isEvenDivisible divisor) arr
        let findElement arr divisor = List.find (isEvenDivisible divisor) arr
        let dividePair (a, b) = a / b

        sorted
        |> List.filter (existsElement reversed)
        |> List.map ((fun x -> (findElement reversed x), x) >> dividePair)

    input
    |> List.collect (convertLineToList >> handleList)
    |> List.sum

let getResult part (input:string list) =
    match part with
    | A -> getResultA input
    | B -> getResultB input
    |> string
