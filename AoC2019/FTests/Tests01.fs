module Tests01

open FSolutions
open Swensen.Unquote
open Xunit

[<Fact>]
let ``Day 1, part A`` () =
  let actual = Day01.resultPartA
  let expected = 3415076
  test <@ actual = expected @>

[<Fact>]
let ``Day 1, part B`` () =
  let actual = Day01.resultPartB
  let expected = 5119745
  test <@ actual = expected @>
