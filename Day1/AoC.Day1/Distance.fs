module AoC.Day1.Distance

let rec distance (leftList: int list) (rightList: int list) = 
    match List.sort leftList, List.sort rightList with
    | [], [] -> 0
    | [a], [b] -> System.Math.Abs(a-b)
    | (a :: aTail), (b :: bTail) -> System.Math.Abs(a-b) + (distance aTail bTail)
    | _, _ -> raise <| System.NotImplementedException("")