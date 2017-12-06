module Day05

let getResultAB getNewValue input =
    let scanFunc getNewValue (map, index, stepCount) _ =
        match Map.tryFind index map with
        | Some value -> ((Map.add index (getNewValue value) map), index + value, stepCount + 1)
        | None       -> (map, -1, stepCount + 1)

    let map =
        input
        |> List.mapi (fun index number -> (index, number |> int))
        |> Map.ofList

    Seq.initInfinite id
    |> Seq.scan (scanFunc getNewValue) (map, 0, -1)
    |> Seq.find (fun (_, index, _) -> index = -1)
    |> fun (_, _, stepCount) -> stepCount

let getResultA =
    getResultAB ((+) 1)

let getResultB =
    getResultAB (fun x -> x + (if x >= 3 then -1 else 1))

let getResult part (input:string list) =
    match part with
    | A -> getResultA input
    | B -> getResultB input
    |> string
