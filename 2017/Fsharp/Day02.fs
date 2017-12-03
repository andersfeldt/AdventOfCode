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

let handleArray numbers =
    let isEvenDivisible small large =
        if small = large then
            false
        else
            large % small = 0

    let sorted = Array.sort numbers
    let reversed = Array.rev sorted
    let existsElement arr divisor = Array.exists (isEvenDivisible divisor) arr
    let findElement arr divisor = Array.find (isEvenDivisible divisor) arr

    sorted
    |> Array.filter (existsElement reversed)
    |> Array.map (fun x -> (findElement reversed x), x)
    |> Array.map (fun (a, b) -> a / b)

let getResultB (input:string[]) =
    input
    |> Array.map convertLineToArray
    |> Array.map handleArray
    |> Array.concat
    |> Array.sum
    |> string

let getResult part (input:string[]) =
    match part with
    | A -> getResultA input
    | B -> getResultB input
