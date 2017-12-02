module Day02

open System

let getResultA (input:string[]) =
    let convertLineToArray (s:string) =
        s.Split([|' '; '\t'|], StringSplitOptions.RemoveEmptyEntries)
        |> Array.map int

    let getDifference (line:string) =
        let array = convertLineToArray line
        (Array.max array) - (Array.min array)

    input
    |> Array.sumBy getDifference
    |> string

let getResult part (input:string[]) =
    match part with
    | A -> getResultA input
    | B -> failwith "Not implemented yet"
