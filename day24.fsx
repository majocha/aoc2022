#r "nuget:FSharpPlus"
#time "on"
open FSharpPlus

let input = System.IO.File.ReadAllLines "24.txt"
let sizeX = input[0].Length - 2
let sizeY = input.Length - 2

let inline (++) (x, y) (dx, dy) = x + dx, y + dy

type BlizzardMap(a: int[,], d) =
    let dx, dy = d
    let sizeX, sizeY = a.GetLength 0, a.GetLength 1
    let wrap x size = if (x % size) < 0 then (x % size) + size else x % size
    let wrap2 (x, y) = wrap x sizeX, wrap y sizeY
    override _.ToString() = sprintf "%A" a
    member _.AtTime p t =
        let xt, yt = p ++ (dx * t, dy * t) |> wrap2
        a[xt, yt]

let pointAtTime =
    let countChar c x y = if input[y + 1][x + 1] = c then 1 else 0
    let init c = Array2D.init sizeX sizeY (countChar c)
    let bs = 
        [
            '<', (-1, 0)
            '>', (1, 0)
            '^', (0, -1)
            'v', (0, 1)
        ] |> map (fun (c, d) -> BlizzardMap((init c), d))
    fun p t -> bs |> List.sumBy (fun b -> b.AtTime p t)


let vis t =
    use f = new System.IO.StreamWriter "vis24.txt"
    for y in 0..sizeY - 1 do
        for x in 0..sizeX - 1 do
            let v = pointAtTime (x, y) t
            fprintf f "%c" (if v = 0 then '.' elif v = 1 then '+' else '#')
        fprintfn f ""


let visn n =
    for t in 1..n do
        vis t
        Async.Sleep 500 |> Async.RunSynchronously

visn 10

vis 0
