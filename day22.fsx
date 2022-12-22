#r "nuget:FSharpPlus"
#time "on"
open FSharpPlus

let input = System.IO.File.ReadAllLines "22.txt"

let mapInput = input |> takeWhile (fun line -> line <> "")
let boardWidth = mapInput |> map String.length |> maximum
let boardHeight = mapInput |> length
let paddedMapInput = mapInput |> map (fun s -> s.PadRight(boardWidth))
let board = Array.init boardHeight (fun y -> Array.init boardWidth (fun x -> printfn "%d %d" y x; paddedMapInput[y][x]))
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

parseProgram [] programInput


