module Day02

open System

let convertLineToArray (s:string) =
    s.Split([|' '; '\t'|], StringSplitOptions.RemoveEmptyEntries)
    |> Array.map int

let getResultA (input:string[]) =
    let getDifference (line:string) =
        let array = convertLineToArray line
        (Array.max array) - (Array.min array)

    input
    |> Array.sumBy getDifference
    |> string

let getResultB (input:string[]) =
    let handleArray numbers =
        let isEvenDivisible small large =
            match small = large with
            | true  -> false
            | false -> large % small = 0

        let sorted = Array.sort numbers
        let reversed = Array.rev sorted
        let existsElement arr divisor = Array.exists (isEvenDivisible divisor) arr
        let findElement arr divisor = Array.find (isEvenDivisible divisor) arr
        let dividePair (a, b) = a / b

        sorted
        |> Array.filter (existsElement reversed)
        |> Array.map ((fun x -> (findElement reversed x), x) >> dividePair)

    input
    |> Array.collect (convertLineToArray >> handleArray)
    |> Array.sum
    |> string

let getResult part (input:string[]) =
    match part with
    | A -> getResultA input
    | B -> getResultB input
