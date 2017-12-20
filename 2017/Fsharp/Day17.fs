module Day17

let getResultA (input:string) =
    let insert value index list =
        list
        |> List.splitAt index
        |> (fun (a, b) -> List.concat [a; [value]; b])

    let stepSize = input |> int

    let folder (buffer, currentPosition) value =
        let insertPosition = ((currentPosition + stepSize) % (List.length buffer)) + 1
        let newBuffer = insert value insertPosition buffer
        (newBuffer, insertPosition)

    let buffer, position =
        [1..2017]
        |> List.fold folder ([0], 0)

    buffer.[position + 1]

let getResult part (input:string list) =
    match part with
    | A -> getResultA input.[0]
    | B -> failwith "Not implemented yet"
    |> string
