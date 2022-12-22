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

let wrap (x,y) = (x + boardWidth) % boardWidth, (y + boardHeight) % boardHeight

let inline (++) (x, y) (dx, dy) = (x + dx, y + dy) |> wrap

let getTile (x,y) = board[x, y]

let rec nextInDir p dir =
    let p = p ++ dirMove dir
    match getTile p  with
    | ' ' -> nextInDir p dir
    | t -> t, p

let vis = Array2D.copy board

let rec forward p dir n =
    if n = 0 then p else
        let x, y = p
        vis[x, y] <- match dir with | Dir.Right -> '>' | Dir.Down -> 'v' | Dir.Left -> '<' | _ -> '^'
        match nextInDir p dir with 
        | '#', _ -> p
        | _ , p' -> forward p' dir (n - 1)

let rec enterPass p dir = function
    | Forward n :: rest -> enterPass (forward p dir n) dir rest
    | t :: rest -> enterPass p (turn dir t) rest
    | [] -> p, dir

let startPos = board[*, 0] |> Array.findIndex (fun c -> c = '.'), 0

let printVis () =
    use f = new System.IO.StreamWriter("vis22.txt")
    for y in 0..boardHeight-1 do
        for x in 0..boardWidth-1 do
            f.Write(vis[x, y])
        f.Write("\n")

let partOne = 
    let (x, y), d = enterPass startPos Dir.Right program
    printVis()
    1000 * (y + 1) + 4 * (x + 1) + (int d)


let cubeCuts side = [
    [
        [ for y in [side .. -1 .. 0] -> Dir.Left, (side * 2, y) ]
        [ for x in [side * 2 .. side * 3 - 1] -> Dir.Up, (x, 0) ]
        [ for y in 0 .. side * 2 - 1 -> Dir.Right, (3 * side - 1, y) ]
    ] |> List.concat

    [
        [ for x in side * 3 .. side * 4 - 1 -> Dir.Up, (x, side * 2) ]
        [ for y in side * 2 .. side * 3 - 1 -> Dir.Right, (3 * side - 1, y) ]
        [ for x in [4 * side - 1 .. -1 .. 2 * side] -> Dir.Down, (x, 3 * side - 1) ]
        [ for y in [3 * side - 1 .. -1 .. 2 * side] -> Dir.Left, (2 * side, y) ]
    ] |> List.concat

    [
        [ for x in [2 * side - 1 .. -1 .. 0] -> Dir.Down, (x, 2 * side - 1) ]
        [ for y in [side * 2 - 1 .. -1 ..  side] -> Dir.Left, (0, y) ]
        [ for x in 0 .. side * 2 - 1 -> Dir.Up, (x, side) ]
    ] |> List.concat
]


