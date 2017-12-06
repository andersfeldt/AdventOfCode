module Day01

let parseChar (c:char) = (c |> int) - ('0' |> int)
let isEqualPair (x, y) = x = y

let getResultA (s:string) =
    let numbers =
        s.ToCharArray()
        |> Array.toList
        |> List.map parseChar

    let toPair (x:'t list) = x.[0], x.[1]

    (List.last numbers) :: numbers
    |> List.windowed 2
    |> List.map toPair
    |> List.filter isEqualPair
    |> List.sumBy fst

let getResultB (s:string) =
    let numbers =
        s.ToCharArray()
        |> Array.toList
        |> List.map parseChar

    let halfLength = numbers.Length / 2
    let firstHalf = numbers |> List.take halfLength
    let secondHalf = numbers |> List.skip halfLength

    List.zip firstHalf secondHalf
    |> List.filter isEqualPair
    |> List.sumBy fst
    |> (*) 2

let getResult part (input:string list) =
    match part with
    | A -> getResultA input.[0]
    | B -> getResultB input.[0]
    |> string
