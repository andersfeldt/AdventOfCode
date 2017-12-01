module Day01

let getResultA (s:string) =
    let numbers =
        s.ToCharArray()
        |> Array.toSeq
        |> Seq.map (fun c -> (c |> int) - ('0' |> int))

    (Seq.last numbers) :: (Seq.toList numbers)
    |> List.windowed 2
    |> List.map (fun x -> x.[0], x.[1])
    |> List.filter (fun (x, y) -> x = y)
    |> List.sumBy fst
    |> string


let getResult part (input:string[]) =
    match part with
    | A -> getResultA input.[0]
    | B -> "bar"
