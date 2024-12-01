module AoC.Day1.Distance

let distance leftList rightList = 
    match leftList, rightList with
    | [], [] -> 0
    | [a], [b] -> a-b
    | _, _ -> raise <| System.NotImplementedException()