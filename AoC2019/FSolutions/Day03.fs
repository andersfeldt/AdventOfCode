module FSolutions.Day03

let input = PuzzleInput.lines 3

// Part A

type Direction = Up | Down | Left | Right

type Position = int * int

type Instruction =
  {
    Direction: Direction
    Steps: int
  }

type State =
  {
    Current: Position
    Visited: Set<Position>
  }

let step position direction offset =
  let x, y = position
  match direction with
  | Up -> x, y + offset
  | Down -> x, y - offset
  | Left -> x - offset, y
  | Right -> x + offset, y

let steps position instr : Position list =
  [1 .. instr.Steps]
  |> List.map (step position instr.Direction)

let move (state: State) (instr: Instruction) : State =
  let newPositions = steps state.Current instr
  let newCurrent =
    newPositions
    |> List.rev
    |> List.head
  let newVisited =
    newPositions
    |> Set.ofList
    |> Set.union state.Visited
  { Current = newCurrent; Visited = newVisited }

let run (instrs: Instruction list) : Set<Position> =
  let initialState = { Current = (0, 0); Visited = Set.empty }
  let finalState =
    instrs
    |> List.fold move initialState
  finalState.Visited

let parse (input: string) : Instruction list =
  let parseDir c =
    match c with
    | 'U' -> Up
    | 'D' -> Down
    | 'L' -> Left
    | 'R' -> Right
    | _ -> failwith "Invalid puzzle"
  let parseInstr (s: string) =
    { Direction = parseDir s.[0]; Steps = s.[1..] |> int }
  input.Split(',')
  |> Array.map parseInstr
  |> Array.toList

let distance position =
  match position with
  | x, y -> abs x + abs y

let resultPartA =
  let visitedSets =
    input
    |> List.map (parse >> run)
  Set.intersect visitedSets.[0] visitedSets.[1]
  |> Set.map distance
  |> Set.minElement
