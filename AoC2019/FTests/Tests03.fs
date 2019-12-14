module Tests03

open FSolutions
open Swensen.Unquote
open Xunit

[<Fact>]
let ``Day 3, part A`` () =
  let actual = Day03.resultPartA
  let expected = 627
  test <@ actual = expected @>
