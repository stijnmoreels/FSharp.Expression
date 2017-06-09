module FSharp.Expression.Tests

open FSharp.Expression
open FsCheck.Xunit
open Xunit

[<Fact>]
let ``hello returns 42`` () =
  let result = Library.hello 42
  printfn "%i" result
  42 = result
