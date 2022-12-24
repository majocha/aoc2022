#r "nuget:FSharpPlus"
#time "on"
open FSharpPlus

let input = System.IO.File.ReadAllLines "24.txt"
let sizeX = input[0].Length - 2
let sizeY = input.Length - 2

type BlizzardMap(c, d) =
    let countChar c x y = if input[y + 1][x + 1] = c then 1 else 0
    let a = Array2D.init sizeX sizeY (countChar c)
    let dx, dy = d
    let sizeX, sizeY = a.GetLength 0, a.GetLength 1
    let wrap x size = if (x % size) < 0 then (x % size) + size else x % size
    let wrap2 (x, y) = wrap x sizeX, wrap y sizeY
    override _.ToString() = sprintf "%A" a
    member _.AtTime (x, y) t =
        let xt, yt = (x - dx * t, y - dy * t) |> wrap2
        a[xt, yt]

let pointAtTime =
    let bs = 
        [
            '<', (-1, 0)
            '>', (1, 0)
            '^', (0, -1)
            'v', (0, 1)
        ] |> map BlizzardMap
    fun p t -> bs |> List.sumBy (fun b -> b.AtTime p t)

let moves =
    [
        1, 0
        0, 1
        -1, 0
        0, -1
        0, 0
    ]

let rec searchFromTo start finish startTime =

    let inBounds (x, y) = (x >= 0 && x < sizeX && y >= 0 && y < sizeY)
    let validMove t n = n = start || n = finish || n |> inBounds && pointAtTime n (t + 1) = 0
    let getNext p t =
        seq { 
            for m in moves do
                if validMove (t + 1) (p ++ m) then 
                    yield p ++ m
        }

    let rec search = 
        fun ((x, y) as p) t ->
            if p = finish then t + 1
            else 
                let limit = sizeX + sizeY * 5 + startTime
                seq {
                    if t < limit then 
                        for next in getNext p t do
                            yield search next (t + 1)
                    yield System.Int32.MaxValue
                } |> minimum
        |> memoizeN

    search start startTime

let start = 0, -1
let finish = sizeX - 1, sizeY

let partOne = searchFromTo start finish 0
let partTwo = partOne |> searchFromTo finish start |> searchFromTo start finish
