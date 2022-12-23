#r "nuget:FSharpPlus"
#time "on"
open FSharpPlus

let input = System.IO.File.ReadAllLines "20.txt" |> map (sscanf "%d")

let size = input |> length

let wrap size i =
    let i = i % size
    if i < 0 then i + size  else i

let moveWrap i  = wrap (size - 1) i
let countWrap = wrap size

let file = input |> Array.indexed |> toList

let vis file =
    printfn "%A" (file |> List.map snd)

let indexOf file nth = 
    file |> List.findIndex (fun (n, x) -> n = nth)

let dxOf file i = file |> List.item i |> snd

let insertAt' i e file =
    if i = 0 then file @ [e]
    else file |> List.insertAt i e

let move file i dx =
    let e = file |> List.item i
    file |> List.deleteAt i |> insertAt' (moveWrap (i + dx)) e
    //|> tap vis

let mixOne file nth =
    let i = indexOf file nth
    let dx = dxOf file i
    move file i dx

let mix file =
    [0 .. size - 1] |> List.fold mixOne file 
    


let mixed = mix file |> List.map snd
let zero = mixed |> List.findIndex ((=) 0)

[
    mixed[countWrap (zero + 1000)]
    mixed[countWrap (zero + 2000)]
    mixed[countWrap (zero + 3000)]
] |> sum

