#r "nuget: FSharpPlus, 1.2.5"
open FSharpPlus
let sections (x1, x2, y1 ,y2) = Set [x1..x2], Set [y1..y2]
let input = System.IO.File.ReadAllLines "4.txt" |> map (sscanf "%d-%d,%d-%d") |> map sections

let fully sets =
     sets ||> Set.intersect = fst sets

let isMatch (a, b) =
    fully (a, b) || fully (b, a)

input |> filter isMatch |> length

let anyOverlap sets =
    sets ||> Set.intersect |> Set.count > 0

input |> filter anyOverlap |> length
