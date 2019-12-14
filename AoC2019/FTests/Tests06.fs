module Tests06

open FSolutions
open Swensen.Unquote
open Xunit

[<Fact>]
let ``Day 6, part A`` () =
  let actual = Day06.resultPartA
  let expected = 344238
  test <@ actual = expected @>

[<Fact>]
let ``Day 6, part B`` () =
  let actual = Day06.resultPartB
  let expected = 436
  test <@ actual = expected @>
