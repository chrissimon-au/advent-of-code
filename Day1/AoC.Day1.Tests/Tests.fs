﻿module AoC.Day1.Tests.Tests

open System
open Xunit
open AoC.Day1.Distance

[<Fact>]
let ``Empty lists return 0 distance`` () =
    Assert.Equal(0, distance [] [])

[<Fact>]
let ``Single element list with same values return 0 distance`` () =
    Assert.Equal(0, distance [3] [3])