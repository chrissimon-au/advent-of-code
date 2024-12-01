module AoC.Day1.ChiefHistorianFinding

let distance (leftList: int list) (rightList: int list) = 
    (List.sort leftList)
    |> List.zip (List.sort rightList)
    |> List.map (fun (left, right) -> System.Math.Abs(left-right))
    |> List.sum

let similarity (leftList: int list) (rightList: int list) = raise <| System.NotImplementedException()