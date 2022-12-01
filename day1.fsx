#r "nuget: FSharpPlus, 1.2.5"
open FSharpPlus

let input = System.IO.File.ReadAllLines "1.txt"
let elves = input |> Seq.split [ [""] ]
let sumOne a: int = a |> map (sscanf "%d") |> sum
elves |> map sumOne |> Seq.sortDescending |> take 3 |> sum


//another approach, more imperative

let elves2 = 
    [
        let mutable cals = 0
        for s in input do
            match tryParse s with
            | Some n -> cals <- cals + n
            | _ ->
                yield cals
                cals <- 0
        yield cals
    ]

elves2 |> List.sortDescending |> take 3 |> sum