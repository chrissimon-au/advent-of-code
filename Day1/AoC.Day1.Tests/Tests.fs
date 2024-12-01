module AoC.Day1.Tests.Tests

open System
open Xunit
open AoC.Day1.ChiefHistorianFinding
open TestData

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

[<Fact>]
let ``Multi-element pre-sorted list distance`` () =
    Assert.Equal(2, distance [3; 4] [4; 5])

[<Fact>]
let ``Multi-element unsorted list distance`` () =
    Assert.Equal(5, distance [10; 4] [4; 5])

[<Fact>]
let ``Sample data from Advent of Code distance`` () =
    Assert.Equal(Sample.distance, distance Sample.left Sample.right)    

[<Fact>]
let ``Test data from Advent of Code distance`` () =
    Assert.Equal(Test.distance, distance Test.left Test.right)


[<Fact>]
let ``Empty lists return 0 similarity`` () =
    Assert.Equal(0, similarity [] [])

[<Fact>]
let ``Single element list with same values return 0 similarity`` () =
    Assert.Equal(0, similarity [3] [0])

// [<Fact>]
// let ``Single element list with diff values return similarity`` () =
//     Assert.Equal(1, similarity [4] [3])

// [<Fact>]
// let ``Single element list with diff values in other order return similarity`` () =
//     Assert.Equal(1, similarity [3] [4])

// [<Fact>]
// let ``Multi-element pre-sorted list similarity`` () =
//     Assert.Equal(2, similarity [3; 4] [4; 5])

// [<Fact>]
// let ``Multi-element unsorted list similarity`` () =
//     Assert.Equal(5, similarity [10; 4] [4; 5])

// [<Fact>]
// let ``Sample data from Advent of Code similarity`` () =
//     Assert.Equal(Sample.similarity, similarity Sample.left Sample.right)

// [<Fact>]
// let ``Test data from Advent of Code similarity`` () =
//     Assert.Equal(Test.similarity, similarity Test.left Test.right)




