#r "nuget:FSharpPlus"
#time "on"
open FSharpPlus

let input = System.IO.File.ReadAllLines "20.txt" |> map (sscanf "%d" >> int64)

let size = input |> length |> int

let wrap size i =
    let i = i % int64 size |> int
    if i < 0 then i + size  else i

let moveWrap i  = wrap (size - 1) i
let countWrap = wrap size

let file = input |> Array.indexed |> toList

let indexOf file nth = 
    file |> List.findIndex (fun (n, x) -> n = nth)

let dxOf file i = file |> item i |> snd

let insertAt' i e file =
    if i = 0 then file @ [e]
    else file |> List.insertAt i e

let move file i dx =
    let e = file |> item i
    file |> List.deleteAt i |> insertAt' (moveWrap (int64 i + dx)) e

let mixOne file nth =
    let i = indexOf file nth
    let dx = dxOf file i
    move file i dx

let mix file = [0 .. size - 1] |> fold mixOne file 

let result file =
    let mixed = file  |> map snd
    let zero = mixed |> findIndex ((=) 0L) |> int64
    [1000L; 2000L; 3000L] |> List.sumBy (fun n -> mixed |> item (countWrap (zero + n)))

let partOne = file |> mix |> result

module PartTwo =

    let key = 811589153L
    let file = input |> map ((*) key) |> Array.indexed |> toList
    let mix f _ = mix f
    
    let partTwo = [1..10] |> fold mix file |> result
