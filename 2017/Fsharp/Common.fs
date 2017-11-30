module Common

open System

let hasContent s = not (String.IsNullOrWhiteSpace(s))
