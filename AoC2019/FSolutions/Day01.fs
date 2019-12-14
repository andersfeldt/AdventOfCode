module FSolutions.Day01

let input = PuzzleInput.lines 1

// Part A

let calc i =
  let sub2 i = i - 2.

  (i / 3.)
  |> floor
  |> sub2

let resultPartA =
  input
  |> List.sumBy (float >> calc)
  |> int

// Part B

let generator i =
  if i > 0. then Some (i, calc i)
  else None

let elf i =
  calc i
  |> Seq.unfold generator

let fuel i =
  elf i
  |> Seq.sum

let resultPartB =
  input
  |> List.sumBy (float >> fuel)
  |> int
