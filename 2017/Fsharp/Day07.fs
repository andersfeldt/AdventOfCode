module Day07

open System
open System.Text.RegularExpressions

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

type TowerInfo =
    {
        weight: int;
        isTotal: bool;
        subTowers: string list;
    }

let getResultB (input:string list) =
    let originalData =
        let parse line =
            let parseLeaf (regexMatch:Match) =
                {
                    weight = regexMatch.Groups.[2].Value |> int;
                    isTotal = true;
                    subTowers = List.empty<string>
                }
            let parseSubTower (regexMatch:Match) =
                let parseCaptures (captures:CaptureCollection) =
                    captures
                    |> Seq.cast<Capture>
                    |> Seq.map (fun c -> c.Value)
                    |> List.ofSeq
                {
                    weight = regexMatch.Groups.[2].Value |> int;
                    isTotal = false;
                    subTowers = regexMatch.Groups.[6].Captures |> parseCaptures
                }

            let regexMatch = Regex.Match(line, @"([a-z]+) \((\d+)\)( -> ((([a-z]+),?\s?)+))?")
            match regexMatch.Groups.[4].Success with
            | true  -> (regexMatch.Groups.[1].Value, parseSubTower regexMatch)
            | false -> (regexMatch.Groups.[1].Value, parseLeaf regexMatch)

        input
        |> List.map parse
        |> Map.ofList

    let rec calculateAllWeights (data:Map<string, TowerInfo>) tower =
        let folder data' subTower =
            calculateAllWeights data' subTower

        let calculateWeightForSubTowers data' tower towerInfo =
            let updatedData =
                towerInfo.subTowers
                |> List.fold folder data'
            let weight =
                towerInfo.subTowers
                |> List.sumBy (fun tower' -> updatedData.[tower'].weight)
                |> (+) towerInfo.weight
            updatedData |> Map.add tower { towerInfo with isTotal = true; weight = weight }

        let towerInfo = data.[tower]
        match towerInfo.isTotal with
        | true  -> data
        | false -> calculateWeightForSubTowers data tower towerInfo

    let allWeightsData = calculateAllWeights originalData (getResultA input)

    let getWeightsForAllSubTowers (data:Map<string, TowerInfo>) (tower, towerInfo) =
        let subTowerWeights =
            towerInfo.subTowers
            |> List.map (fun subTower -> data.[subTower].weight)
        (tower, towerInfo, subTowerWeights, (subTowerWeights |> Seq.distinct |> Seq.length) > 1)

    let calculateNewWeight (subTowers:string list) subTowerWeights =
        let index, diff =
            match (subTowerWeights |> List.sort) with
            | x::y::_ when x < y -> subTowerWeights |> List.findIndex ((=) x), y - x
            | x::xs              -> subTowerWeights |> List.findIndex ((=) (List.max xs)), x - List.max xs
            | _                  -> failwith "Should not happen..."
        originalData.[subTowers.[index]].weight + diff

    allWeightsData
    |> Map.toList
    |> List.filter (fun (_, towerInfo) -> towerInfo.subTowers |> List.isEmpty |> not)
    |> List.map (getWeightsForAllSubTowers allWeightsData)
    |> List.find (fun (_, _, _, isUnbalanced) -> isUnbalanced)
    |> (fun (_, towerInfo, subTowerWeights, _) -> calculateNewWeight towerInfo.subTowers subTowerWeights)
    |> string

let getResult part (input:string list) =
    match part with
    | A -> getResultA input
    | B -> getResultB input
