module Tests10

open FSolutions
open Swensen.Unquote
open Xunit

[<Fact>]
let ``Day 10, part A`` () =
  let actual = Day10.resultPartA
  let expected = 263
  test <@ actual = expected @>
