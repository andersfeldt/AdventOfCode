module Day05

let getResultA input =
    let scanFunc (map, index, stepCount) _ =
        match Map.tryFind index map with
        | Some value -> ((Map.add index (value + 1) map), index + value, stepCount + 1)
        | None       -> (map, -1, stepCount + 1)

    let map =
        input
        |> Array.mapi (fun index number -> (index, number |> int))
        |> Map.ofArray

    Seq.initInfinite id
    |> Seq.scan scanFunc (map, 0, -1)
    |> Seq.find (fun (_, index, _) -> index = -1)
    |> (fun (_, _, stepCount) -> stepCount |> string)

let getResult part (input:string[]) =
    match part with
    | A -> getResultA input
    | B -> failwith "Not implemented yet"
