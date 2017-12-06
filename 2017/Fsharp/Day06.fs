module Day06

open System

let getResultAB (input:string) =
    let initialConfig =
        input.Split([|' '; '\t'|], StringSplitOptions.RemoveEmptyEntries)
        |> List.ofArray
        |> List.map int

    let update config =
        let length = config |> List.length

        let changesByIndex =
            let maxValue = config |> List.max
            let selectedIndex = config |> List.findIndex ((=) maxValue)

            [for i in [1..maxValue] do
                yield (selectedIndex, -1)
                yield ((i + selectedIndex) % length, 1)]
            |> List.groupBy fst
            |> List.map (fun (index, values) -> (index, values |> List.sumBy snd))

        let missingChangesByIndex =
            let existingIndices = changesByIndex |> List.map fst
            let filterExistingIndices index =
                match existingIndices |> List.contains index with
                | true  -> None
                | false -> Some index

            [0..(length - 1)]
            |> List.choose filterExistingIndices
            |> List.map (fun index -> (index, 0))

        changesByIndex
        |> List.append missingChangesByIndex
        |> List.sortBy fst
        |> List.map snd
        |> List.zip config
        |> List.map (fun (a, b) -> a + b)

    let serialize (values:int list) = String.Join(" ", values)

    let scanFunc (history, config, count, _) _ =
        let nextConfig = update config
        let nextConfigSerialized = serialize nextConfig

        match List.contains nextConfigSerialized history with
        | true  -> (history, nextConfig, count + 1, true)
        | false -> ((nextConfigSerialized :: history), nextConfig, count + 1, false)

    Seq.initInfinite id
    |> Seq.scan scanFunc (List.empty<string>, initialConfig, 0, false)
    |> Seq.find (fun (_, _, _, isLoop) -> isLoop)
    |> fun (_, lastConfig, count, _) -> (serialize lastConfig, count)

let getResultA input =
    getResultAB input
    |> snd
    |> string

let getResultB input =
    getResultAB input
    |> fst
    |> getResultAB
    |> snd
    |> fun i -> i - 1
    |> string

let getResult part (input:string[]) =
    match part with
    | A -> getResultA input.[0]
    | B -> getResultB input.[0]
