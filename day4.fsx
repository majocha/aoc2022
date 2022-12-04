#r "nuget: FSharpPlus, 1.2.5"
open FSharpPlus

let input = System.IO.File.ReadAllLines "4.txt" |> map (sscanf "%d-%d,%d-%d")

let fully (x1, x2, y1 ,y2) =
    (y1 <= x1 && x2 <= y2) || (x1 <= y1 && y2 <= x2)

input |> filter fully |> length

// part two
let anyOverlap (x1, x2, y1, y2) =
    if x1 < y1 then x2 >= y1 else y2 >= x1

input |> filter anyOverlap |> length