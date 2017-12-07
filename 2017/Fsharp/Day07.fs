module Day07

open System

let getResultA (input:string list) =
    let lineToPair (line:string) =
        match (line.Split("(->), 0123456789".ToCharArray(), StringSplitOptions.RemoveEmptyEntries) |> List.ofArray) with
        | [x]   -> (x, List.empty<string>)
        | x::xs -> (x, xs)
        | []    -> failwith "should not happen..."
    let listOfPairs =
        input
        |> List.map lineToPair
    let allNames =
        listOfPairs
        |> List.map fst
        |> Set.ofList
    let leavesAndSubTrees =
        listOfPairs
        |> List.collect snd
        |> List.distinct
        |> Set.ofList

    Set.difference allNames leavesAndSubTrees
    |> Set.toSeq
    |> Seq.exactlyOne

let getResult part (input:string list) =
    match part with
    | A -> getResultA input
    | B -> failwith "Not implemented yet"
