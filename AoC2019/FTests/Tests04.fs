module Tests04

open FSolutions
open Swensen.Unquote
open Xunit

[<Fact>]
let ``Day 4, part A`` () =
  let actual = Day04.resultPartA
  let expected = 1169
  test <@ actual = expected @>

[<Fact>]
let ``Day 4, part B`` () =
  let actual = Day04.resultPartB
  let expected = 757
  test <@ actual = expected @>
