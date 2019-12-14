module FSolutions.Day06

let input = PuzzleInput.lines 6

// Part A

type Tree =
  | Node of Name: string * Value: int * Children: Tree list
  | Leaf of Name: string * Value: int

let buildTree (relations: Map<string, string list>) (root: string) : Tree =
  let rec buildTree' (relations: Map<string, string list>) (value: int) (root: string) : Tree =
    match relations |> Map.tryFind root with
    | None
    | Some [] -> Leaf (root, value)
    | Some rels ->
      let trees =
        rels
        |> List.map (buildTree' relations (value + 1))
      Node (root, value, trees)
  buildTree' relations 0 root

let parse (lines: string list) : Map<string, string list> =
  let parse' (s:string) =
    match s.Split(')') with
    | [| a; b |] -> a, b
    | _ -> failwith "Invalid"
  let folder (currentMap: Map<string, string list>) (to', from) =
    let newList =
      match currentMap |> Map.tryFind to' with
      | None -> [from]
      | Some xs -> from::xs
    currentMap |> Map.add to' newList
  lines
  |> List.map parse'
  |> List.fold folder Map.empty

let extractValues tree =
  let rec loop tree =
    seq {
      match tree with
      | Node (_, v, c) ->
        yield v
        yield! (c |> Seq.collect loop)
      | Leaf (_, v) -> yield v
    }
  loop tree

let relations = parse input

let resultPartA =
  buildTree relations "COM"
  |> extractValues
  |> Seq.sum

// Part B

let reversedRelations =
  relations
  |> Map.toSeq
  |> Seq.collect (fun (parent, children) -> children |> List.map (fun child -> (child, parent)))
  |> Map.ofSeq

let pathToRoot (reversedRelations: Map<string, string>) (key: string) : string list =
  let generator (state: string list) : (string * string list) option =
    match state with
    | [] -> None
    | child::_ ->
      match reversedRelations |> Map.tryFind child with
      | Some parent -> Some (parent, parent :: state)
      | None -> None
  List.unfold generator [key]

let pathToYou = pathToRoot reversedRelations "YOU" |> List.rev
let pathToSan = pathToRoot reversedRelations "SAN" |> List.rev

let closestCommonRootIndex =
  Seq.zip pathToYou pathToSan
  |> Seq.findIndex (fun (a, b) -> a <> b)

let resultPartB =
  let lengthToYou = List.length pathToYou - closestCommonRootIndex
  let lengthToSan = List.length pathToSan - closestCommonRootIndex
  lengthToYou + lengthToSan
