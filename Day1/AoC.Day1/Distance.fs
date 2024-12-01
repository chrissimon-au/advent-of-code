module AoC.Day1.Distance

let distance (leftList: int list) (rightList: int list) = 
    match leftList, rightList with
    | [], [] -> 0
    | [a], [b] -> System.Math.Abs(a-b)
    | _, _ -> raise <| System.NotImplementedException()