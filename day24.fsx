#r "nuget:FSharpPlus"
#time "on"
open FSharpPlus

let input = System.IO.File.ReadAllLines "24.txt"
let sizeX = input[0].Length - 2
let sizeY = input.Length - 2


type BlizzardMap(c: char, a: int[,], d) =
    let dx, dy = d
    let sizeX, sizeY = a.GetLength 0, a.GetLength 1
    let wrap x size = if (x % size) < 0 then (x % size) + size else x % size
    let wrap2 (x, y) = wrap x sizeX, wrap y sizeY
    override _.ToString() = sprintf "%A" a
    member _.AtTime (x, y) t =
        let xt, yt = (x - dx * t, y - dy * t) |> wrap2
        a[xt, yt]
    member this.CharAtTime p t = if this.AtTime p t = 1 then Some c else None

let pointAtTime =
    let countChar c x y = if input[y + 1][x + 1] = c then 1 else 0
    let init c = Array2D.init sizeX sizeY (countChar c)
    let bs = 
        [
            '<', (-1, 0)
            '>', (1, 0)
            '^', (0, -1)
            'v', (0, 1)
        ] |> map (fun (c, d) -> BlizzardMap(c, (init c), d))
    fun p t -> bs |> List.sumBy (fun b -> b.AtTime p t)

let CharAtTime =
    let countChar c x y = if input[y + 1][x + 1] = c then 1 else 0
    let init c = Array2D.init sizeX sizeY (countChar c)
    let bs = 
        [
            '<', (-1, 0)
            '>', (1, 0)
            '^', (0, -1)
            'v', (0, 1)
        ] |> map (fun (c, d) -> BlizzardMap(c, (init c), d))
    fun p t ->
        let chars = bs |> List.map (fun b -> b.CharAtTime p t) |> List.filter (Option.isSome)
        if chars.Length = 0 then '.' 
        elif chars.Length > 1 then (chars.Length.ToString() |> Seq.item 0) 
        else chars |> List.exactlyOne |> Option.get


let vis t p =
    use f = new System.IO.StreamWriter "vis24.txt"
    for y in 0..sizeY - 1 do
        for x in 0..sizeX - 1 do
            let c = if p = (x, y) then 'E' else CharAtTime (x, y) t 
            fprintf f "%c" c
        fprintfn f ""

let visAll moves =
    for t, m in moves |> List.indexed do
        vis (t + 1) m
        Async.Sleep 500 |> Async.RunSynchronously


let moves =
    [
        1, 0
        0, 1
        -1, 0
        0, -1
        0, 0
    ]

let isStart (x,y) = (x, y) = (0, -1)
let isFinish (x,y) = (x, y) = (sizeX - 1, sizeY)
let inBounds (x, y) =
    isStart (x, y) || isFinish (x, y) || (x >= 0 && x < sizeX && y >= 0 && y < sizeY)

let validMove t n =
    isFinish n || n |> inBounds && pointAtTime n (t + 1) = 0
let getNext p t =
    seq { 
        for m in moves do
            if validMove (t + 1) (p ++ m) then 
                yield p ++ m
    }

let rec search = 
    fun ((x, y) as p) t ->
        if isFinish p then t + 1
        else 
            let limit = sizeX + sizeY * 5
            seq {
                if t < limit then 
                    for next in getNext p t do
                        yield search next (t + 1)
                yield System.Int32.MaxValue
            } |> minimum
    |> memoizeN

let res = search (0, -1) 1
// res |> length
// res |> rev

// vis 17 (5, 3)
// validMove 17 (5, 4)
// sizeX + sizeY


