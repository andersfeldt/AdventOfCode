module Day24

type Bridge =
    {
        currentPort: int;
        strength: int;
        length: int;
    }

let parse input =
    let parseLine (line:string) =
        line.Split('/')
        |> Array.map int
        |> (fun x -> x.[0], x.[1])

    input
    |> List.map parseLine

let canConnectTo value (a, b) =
    value = a || value = b

let buildNewBridge bridge (a, b) =
    {
        currentPort = if bridge.currentPort = a then b else a;
        strength = bridge.strength + a + b;
        length = bridge.length + 1;
    }

let except items itemToExclude =
    let indexToExclude = List.findIndex (fun item -> item = itemToExclude) items
    let first, second = List.splitAt indexToExclude items
    List.append first (List.tail second)

let rec buildAllBridges components bridge =
    components
    |> List.partition (canConnectTo bridge.currentPort)
    |> fst
    |> List.map (fun comp -> comp, buildNewBridge bridge comp)
    |> List.collect (fun (comp, newBridge) -> newBridge :: (buildAllBridges (except components comp) newBridge))

let getResultA (input:string list) =
    buildAllBridges (parse input) { currentPort = 0; strength = 0; length = 0; }
    |> List.maxBy (fun bridge -> bridge.strength)
    |> (fun bridge -> bridge.strength)

let getResultB (input:string list) =
    buildAllBridges (parse input) { currentPort = 0; strength = 0; length = 0; }
    |> List.maxBy (fun bridge -> bridge.length)
    |> (fun bridge -> bridge.strength)

let getResult part (input:string list) =
    match part with
    | A -> getResultA input
    | B -> getResultB input
    |> string
