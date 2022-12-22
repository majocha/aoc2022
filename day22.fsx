#r "nuget:FSharpPlus"
#time "on"
open FSharpPlus

let input = System.IO.File.ReadAllLines "22.txt"

let mapInput = input |> takeWhile (fun line -> line <> "")
let boardWidth = mapInput |> map String.length |> maximum
let boardHeight = mapInput |> length
let paddedMapInput = mapInput |> map (fun s -> s.PadRight(boardWidth))
let board = Array2D.init boardWidth boardHeight (fun x y -> paddedMapInput[y][x])
let programInput = input |> skipWhile (fun line -> line <> "") |> skip 1 |> Array.exactlyOne

type Move = Left | Right | Forward of int

let tryMatch pat inp =
    let m = System.Text.RegularExpressions.Regex.Match(inp, $"^({pat}).*")
    if m.Success then Some (m.Groups[1].Value, inp[m.Groups[1].Length..]) else None

let (|Dist|_|) = tryMatch @"\d+" >> Option.map (mapItem1 int)
let (|Turn|_|) input =
    match tryMatch @"L|R" input with Some ("L", rest) -> Some (Left, rest) | Some ("R", rest) -> Some (Right, rest)  | _ -> None

let rec parseProgram acc = function
    | Dist (d, rest) -> parseProgram (Forward d :: acc) rest
    | Turn (dir, rest) -> parseProgram (dir :: acc) rest
    | _ -> acc |> rev

let program  = parseProgram [] programInput

type Dir = Right = 0 | Down = 1 | Left = 2 | Up = 3
let turn d t = 
    let rotate = match t with  Left -> -1 | Right -> 1 | _ -> 0
    ((int d + 4) + rotate) % 4 |> enum<Dir>

let dirMove = function Dir.Right -> 1, 0 | Dir.Down -> 0, 1 | Dir.Left -> -1, 0 | Dir.Up -> 0, -1 | _ -> 0, 0

let getTile (x,y) = board[x, y]

let rec nextInDir p dir =
    let inline wrap (x,y) = (x + boardWidth) % boardWidth, (y + boardHeight) % boardHeight
    let inline (++) (x, y) (dx, dy) = (x + dx, y + dy) |> wrap
    let p = p ++ dirMove dir
    match getTile p  with
    | ' ' -> nextInDir p dir
    | t -> dir, t, p

let vis = Array2D.copy board

let rec forward move p dir n =
    if n = 0 then dir, p else
        let x, y = p
        vis[x, y] <- match dir with | Dir.Right -> '>' | Dir.Down -> 'v' | Dir.Left -> '<' | _ -> '^'
        match move p dir with 
        | _, '#', _ -> dir, p
        | dir, _ , p -> forward move p dir (n - 1)

let rec enterPass move p dir = function
    | Forward n :: rest -> 
        let dir, p = forward move p dir n
        enterPass move p dir rest
    | t :: rest -> enterPass move p (turn dir t) rest
    | [] -> p, dir

let startPos = board[*, 0] |> Array.findIndex (fun c -> c = '.'), 0

let printVis _ =
    // Async.Sleep 700 |> Async.RunSynchronously
    use f = new System.IO.StreamWriter("vis22.txt")
    for y in 0..boardHeight-1 do
        for x in 0..boardWidth-1 do
            f.Write(vis[x, y])
        f.Write("\n")

// let partOne = 
//     let (x, y), d = enterPass nextInDir startPos Dir.Right program
//     printVis()
//     1000 * (y + 1) + 4 * (x + 1) + (int d)

let side = 50

let faces = 
    [
        for y = 0 to boardHeight / side - 1 do
            for x = 0 to boardWidth / side - 1 do
                if getTile (x * side,y * side) <> ' ' then x, y
    ]

let connections =
    let cs =
        [
            (0, Dir.Right), (1, Dir.Left), true
            (0, Dir.Down), (2, Dir.Up), true
            (3, Dir.Right), (4, Dir.Left), true
            (2, Dir.Down), (4, Dir.Up), true
            (3, Dir.Down), (5, Dir.Up), true
            (1, Dir.Down), (2, Dir.Right), true
            (4, Dir.Down), (5, Dir.Right), true
            (1, Dir.Right), (4, Dir.Right), false
            (2, Dir.Left), (3, Dir.Up), true
            (0, Dir.Left), (3, Dir.Left), false
            (0, Dir.Up), (5, Dir.Left), true
            (1, Dir.Up), (5, Dir.Down), true
        ]
    List.concat [ cs; cs |> List.map( fun (e1, e2, b) -> e2, e1, b)]
    |> map (fun (e1, e2, b) -> e1, (e2, b)) |> Map

let edge fid dir =
    let fx, fy = faces[fid]
    match dir with
    | Dir.Up -> [ for x in 0..side - 1 -> x + fx * side, fy * side ]
    | Dir.Down -> [ for x in 0..side - 1 -> x + fx * side, side - 1 + fy * side ]
    | Dir.Left -> [ for y in 0..side - 1 -> fx * side, y + fy * side ]
    | Dir.Right -> [ for y in 0..side - 1 -> fx * side + side - 1, y + fy * side ]

let reverseDir dir = (int dir + 2) % 4 |> enum<Dir>

let cubeWrap p dir =
    match [0..5] |> tryFind (fun f -> edge f dir |> List.contains p) with
    | Some fid ->
        let (f2, d2), r = connections[fid, dir]
        let e1 = edge fid dir
        let e2 = if r then edge f2 d2 else edge f2 d2 |> rev
        let newPos = e2 |> item (e1 |> List.findIndex (fun p' -> p' = p))
        Some (reverseDir d2, newPos)
    | _ -> None

let rec nextInDir2 p dir =
    let inline (++) (x, y) (dx, dy) = (x + dx, y + dy)
    let dir, p = (cubeWrap p dir) |> Option.defaultValue (dir, p ++ dirMove dir)
    dir, getTile p, p

let partTwo =
    // let f = tap (printVis) >> nextInDir2
    let (x, y), d = enterPass nextInDir2 startPos Dir.Right program
    printVis()
    1000 * (y + 1) + 4 * (x + 1) + (int d)

connections.Keys |> toList

