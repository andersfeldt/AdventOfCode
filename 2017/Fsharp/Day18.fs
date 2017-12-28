module Day18

open System
open System.Text.RegularExpressions

type Register =
    | Name  of string
    | Value of int64

type Instruction =
    | Snd of Register
    | Set of string * Register
    | Add of string * Register
    | Mul of string * Register
    | Mod of string * Register
    | Rcv of Register
    | Jgz of Register * Register

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

            let m = Regex.Match(code, @"([a-z]{3}) ([-0-9a-z]+)\s?([-0-9a-z]+)?")
            match (toString m 1) with
            | "snd" -> Snd (toRegister m 2)
            | "set" -> Set (toString m 2, toRegister m 3)
            | "add" -> Add (toString m 2, toRegister m 3)
            | "mul" -> Mul (toString m 2, toRegister m 3)
            | "mod" -> Mod (toString m 2, toRegister m 3)
            | "rcv" -> Rcv (toRegister m 2)
            | "jgz" -> Jgz (toRegister m 2, toRegister m 3)
            | _     -> failwithf "Not supported: %A" code

        input
        |> List.mapi (fun index code -> (int64 index, parse code))
        |> Map.ofList

    let instructions = parseInstructions input

    let initialRegisters =
        input
        |> List.map (fun s -> s.ToCharArray().[4])
        |> List.distinct
        |> List.filter (fun c -> c >= 'a' && c <= 'z')
        |> List.map (fun c -> (string c, 0L))
        |> Map.ofList

    let getValue (registers:Map<string,int64>) register =
        match register with
        | Name name   -> registers.[name]
        | Value value -> value

    let folder (registers, currentIndex, playlist, _) _ =
        let foldSnd x =
            let frequency = getValue registers x
            registers, currentIndex + 1L, (frequency :: playlist), None

        let foldSet x y =
             let value = getValue registers y
             let newRegisters = Map.add x value registers
             newRegisters, currentIndex + 1L, playlist, None

        let foldChange op x y =
            let valueX = registers.[x]
            let valueY = getValue registers y
            let newValue = op valueX valueY
            let newRegisters = Map.add x newValue registers
            newRegisters, currentIndex + 1L, playlist, None

        let foldRcv x =
            let innerRcv value =
                let lastFrequency, newPlaylist =
                    match playlist with
                    | x::xs -> x, xs
                    | _     -> failwith "Not supported"

                match value = 0L with
                | true  -> registers, currentIndex + 1L, newPlaylist, None
                | false -> registers, currentIndex + 1L, newPlaylist, Some lastFrequency

            match getValue registers x with
            | value when value = 0L -> registers, currentIndex + 1L, playlist, None
            | value                 -> innerRcv value

        let foldJgz x y =
            let increment =
                match getValue registers x > 0L with
                | true  -> getValue registers y
                | false -> 1L
            registers, currentIndex + increment, playlist, None

        match instructions.[currentIndex] with
        | Snd x      -> foldSnd x
        | Set (x, y) -> foldSet x y
        | Add (x, y) -> foldChange (+) x y
        | Mul (x, y) -> foldChange (*) x y
        | Mod (x, y) -> foldChange (%) x y
        | Rcv x      -> foldRcv x
        | Jgz (x, y) -> foldJgz x y

    Seq.initInfinite id
    |> Seq.scan folder (initialRegisters, 0L, List.empty, None)
    |> Seq.pick (fun (_, _, _, x) -> x)

let getResult part (input:string list) =
    match part with
    | A -> getResultA input
    | B -> failwith "Not implemented yet"
    |> string
