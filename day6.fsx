#r "nuget: FSharpPlus"
open FSharpPlus
let input = System.IO.File.ReadAllText "6.txt"

let n = 14

let result =
    input |> Seq.windowed n 
    |> map (distinct >> length) 
    |> Seq.indexed
    |> find (fun e -> snd e = n)

fst result + n
