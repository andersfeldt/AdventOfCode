module Day01

let isEqualPair (x, y) = x = y

let getResultA (s:string) =
    let numbers =
        s.ToCharArray()
        |> Array.toSeq
        |> Seq.map (fun c -> (c |> int) - ('0' |> int))

    (Seq.last numbers) :: (Seq.toList numbers)
    |> List.windowed 2
    |> List.map (fun x -> x.[0], x.[1])
    |> List.filter isEqualPair
    |> List.sumBy fst
    |> string

let getResultB (s:string) =
    let numbers =
        s.ToCharArray()
        |> Array.toList
        |> List.map (fun c -> (c |> int) - ('0' |> int))

    let halfLength = numbers.Length / 2
    let firstHalf = numbers |> List.take halfLength
    let secondHalf = numbers |> List.skip halfLength

    List.zip firstHalf secondHalf
    |> List.filter isEqualPair
    |> List.sumBy fst
    |> (fun x -> x * 2)
    |> string

let getResult part (input:string[]) =
    match part with
    | A -> getResultA input.[0]
    | B -> getResultB input.[0]
