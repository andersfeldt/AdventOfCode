module Tests08

open System
open FSolutions
open Swensen.Unquote
open Xunit

[<Fact>]
let ``Day 8, part A`` () =
  let actual = Day08.resultPartA
  let expected = 1935
  test <@ actual = expected @>

[<Fact>]
let ``Day 8, part B`` () =
  let actual = Day08.resultPartB
  let asciiArt =
    [
      " **  **** *    *  * *    "
      "*  * *    *    *  * *    "
      "*    ***  *    *  * *    "
      "*    *    *    *  * *    "
      "*  * *    *    *  * *    "
      " **  *    ****  **  **** "
    ]
  let expected = String.Join('\n', asciiArt)
  test <@ actual = expected @>
