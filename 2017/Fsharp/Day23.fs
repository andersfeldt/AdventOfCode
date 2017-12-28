module Day23

open System
open System.Text.RegularExpressions

type Register =
    | Name  of string
    | Value of int64

type Instruction =
    | Set of string * Register
    | Sub of string * Register
    | Mul of string * Register
    | Jnz of Register * Register

let getResultA (input:string list) =
    let parseInstructions input =
        let parse code =
            let toRegister (m:Match) (index:int) =
                let s = m.Groups.[index].Value
                match Int64.TryParse(s) with
                | (true, i) -> Value i
                | _         -> Name s

            let toString (m:Match) (index:int) =
                m.Groups.[index].Value

            let m = Regex.Match(code, @"([a-z]{3}) ([-0-9a-z]+) ([-0-9a-z]+)")
            match (toString m 1) with
            | "set" -> Set (toString m 2, toRegister m 3)
            | "sub" -> Sub (toString m 2, toRegister m 3)
            | "mul" -> Mul (toString m 2, toRegister m 3)
            | "jnz" -> Jnz (toRegister m 2, toRegister m 3)
            | _     -> failwithf "Not supported: %A" code

        input
        |> List.mapi (fun index code -> (int64 index, parse code))
        |> Map.ofList

    let instructions = parseInstructions input

    let initialRegisters =
        ['a'..'h']
        |> List.map (fun c -> (string c, 0L))
        |> Map.ofList

    let getValue (registers:Map<string,int64>) register =
        match register with
        | Name name   -> registers.[name]
        | Value value -> value

    let folder (registers, currentIndex, count, _) _ =
        let foldSet x y =
             let value = getValue registers y
             let newRegisters = Map.add x value registers
             newRegisters, currentIndex + 1L, count, false

        let foldChange op x y countIncrement =
            let valueX = registers.[x]
            let valueY = getValue registers y
            let newValue = op valueX valueY
            let newRegisters = Map.add x newValue registers
            newRegisters, currentIndex + 1L, count + countIncrement, false

        let foldJnz x y =
            let increment =
                match getValue registers x = 0L with
                | true  -> 1L
                | false -> getValue registers y
            registers, currentIndex + increment, count, false

        match Map.tryFind currentIndex instructions with
        | Some (Set (x, y)) -> foldSet x y
        | Some (Sub (x, y)) -> foldChange (-) x y 0
        | Some (Mul (x, y)) -> foldChange (*) x y 1
        | Some (Jnz (x, y)) -> foldJnz x y
        | None              -> registers, currentIndex, count, true

    Seq.initInfinite id
    |> Seq.scan folder (initialRegisters, 0L, 0, false)
    |> Seq.find (fun (_, _, _, x) -> x)
    |> (fun (_, _, x, _) -> x)

let getResult part (input:string list) =
    match part with
    | A -> getResultA input
    | B -> failwith "Not implemented yet"
    |> string
