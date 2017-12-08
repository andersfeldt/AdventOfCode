module Day08

open System.Text.RegularExpressions

type Instruction =
    {
        registerToModify:string;
        change:int;
        conditionalRegister:string;
        predicate:int -> int -> bool;
        compareValue:int;
    }

let getInstructions input =
    let parseLine line =
        let getPredicate token =
            match token with
            | "<"  -> (<)
            | ">"  -> (>)
            | "<=" -> (<=)
            | ">=" -> (>=)
            | "==" -> (=)
            | "!=" -> (<>)
            | _    -> failwith "Not supported"
        let values =
            Regex.Match(line, @"([a-z]+) (inc|dec) (-?\d+) if ([a-z]+) ([<>!=]+) (-?\d+)").Groups
            |> Seq.cast<Group>
            |> Seq.map (fun g -> g.Value)
            |> Seq.toList
        {
            registerToModify = values.[1];
            change = (if values.[2] = "inc" then 1 else -1) * (values.[3] |> int);
            conditionalRegister = values.[4];
            predicate = getPredicate values.[5];
            compareValue = values.[6] |> int;
        }

    input |> List.map parseLine

let initRegisters instructions =
    instructions
    |> List.map (fun i -> i.registerToModify)
    |> Seq.distinct
    |> Seq.map (fun register -> (register, 0))
    |> Map.ofSeq

let getMaxValue registers =
    registers
    |> Map.toList
    |> List.map snd
    |> List.max

let getResultA (input:string list) =
    let instructions = getInstructions input
    let registers = initRegisters instructions

    let folder (state:Map<string, int>) instruction =
        match instruction.predicate state.[instruction.conditionalRegister] instruction.compareValue with
        | true  -> state |> Map.add instruction.registerToModify (state.[instruction.registerToModify] + instruction.change)
        | false -> state

    instructions
    |> List.fold folder registers
    |> getMaxValue

let getResultB (input:string list) =
    let instructions = getInstructions input
    let registers = initRegisters instructions

    let folder ((state:Map<string, int>), maxValue) instruction =
        match instruction.predicate state.[instruction.conditionalRegister] instruction.compareValue with
        | true  ->
            let newState = state |> Map.add instruction.registerToModify (state.[instruction.registerToModify] + instruction.change)
            newState, max maxValue (newState |> getMaxValue)
        | false -> state, maxValue

    instructions
    |> List.fold folder (registers, 0)
    |> snd

let getResult part (input:string list) =
    match part with
    | A -> getResultA input
    | B -> getResultB input
    |> string
