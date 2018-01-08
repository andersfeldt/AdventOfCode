module Day25

open System
open System.Text.RegularExpressions

type RuleKey =
    {
        state: string;
        value: bool;
    }

type Rule =
    {
        newValue: bool;
        move: int -> int;
        nextState: string;
    }

let getResultA (input:string list) =
    let parse (input:string list) =
        let pattern = @"In state ([A-Z]):\s+(If the current value is (\d)[.:\s-]+Write the value (\d)[.:\s-]+Move one slot to the (left|right)[.:\s-]+Continue with state ([A-Z])[.:\s-]+)+"
        let createRulesWithKey (m:Match) =
            let ruleKey (i:int) =
                {
                    state = m.Groups.[1].Value
                    value = m.Groups.[3].Captures.[i].Value = "1"
                }
            let rule (i:int) =
                {
                    newValue     = m.Groups.[4].Captures.[i].Value = "1"
                    move      = if m.Groups.[5].Captures.[i].Value = "left" then (fun x -> x - 1) else ((+) 1)
                    nextState    = m.Groups.[6].Captures.[i].Value
                }
            [(ruleKey 0, rule 0); (ruleKey 1, rule 1)]

        let ruleMap =
            Regex.Matches(String.Concat input, pattern)
            |> Seq.cast<Match>
            |> Seq.collect createRulesWithKey
            |> Map.ofSeq

        let initialState, totalSteps =
            let m = Regex.Match(input |> List.take 2 |> String.Concat, @"Begin in state ([A-Z])\.Perform a diagnostic checksum after (\d+) steps")
            m.Groups.[1].Value, m.Groups.[2].Value |> int

        ruleMap, initialState, totalSteps

    let folder (ruleMap:Map<RuleKey,Rule>) (currentState, (tape:Map<int,bool>), currentIndex) _ =
        let currentValue =
            match Map.tryFind currentIndex tape with
            | Some b -> b
            | _      -> false
        let rule = ruleMap.[{ state = currentState; value = currentValue; }]
        rule.nextState, Map.add currentIndex rule.newValue tape, rule.move currentIndex

    let ruleMap, initialState, totalSteps = parse input

    Seq.initInfinite id
    |> Seq.take totalSteps
    |> Seq.fold (folder ruleMap) (initialState, [(0, false)] |> Map.ofList, 0)
    |> (fun (_, x, _) -> x)
    |> Map.toList
    |> List.map snd
    |> List.partition id
    |> fst
    |> List.length

let getResult part (input:string list) =
    match part with
    | A -> getResultA input
    | B -> failwith "Not implemented yet"
    |> string
