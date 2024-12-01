module AoC.Day1.Tests.Tests

open System
open Xunit
open AoC.Day1.Distance

[<Fact>]
let ``Empty lists return 0 distance`` () =
    Assert.Equal(0, distance [] [])

[<Fact>]
let ``Single element list with same values return 0 distance`` () =
    Assert.Equal(0, distance [3] [3])

[<Fact>]
let ``Single element list with diff values return distance`` () =
    Assert.Equal(1, distance [4] [3])

[<Fact>]
let ``Single element list with diff values in other order return distance`` () =
    Assert.Equal(1, distance [3] [4])