#r "nuget: FSharpPlus, 1.2.5"
open FSharpPlus

let input = System.IO.File.ReadAllLines "7.txt" |> toList

type CdParam = Root | Up | Down of string
let cddir = function "/" -> Root | ".." -> Up | dir -> Down dir
let (|Cd|_|)  = trySscanf "$ cd %s" >> Option.map cddir
let (|Ls|_|) = function "$ ls" -> Some () | _ -> None
let (|File|_|) = trySscanf "%d %s" >> Option.map fst

let sumFiles total = function File size -> total + size | _ -> total

let rec sumDirs result path = function
    | Cd Root :: rest -> sumDirs result ["/"] rest
    | Cd Up :: rest -> sumDirs result (path |> skip 1) rest
    | Cd (Down dir) :: rest -> sumDirs result (dir :: path) rest
    | Ls :: rest ->
        let lines = rest |> takeWhile (fun s -> not (s.StartsWith "$")) |> toList
        let total = lines |> fold sumFiles 0
        let pathStr = path |> rev |> String.intercalate "/"
        sumDirs ((pathStr, total) :: result) path (rest |> skip lines.Length) 
    | _ -> result

let result = sumDirs [] [] input

let paths = result |> map fst

let dirSizes = 
    [ 
        for path in paths do
            result
            |> filter (fun (p, _) -> p.StartsWith path)
            |> List.sumBy snd
    ] 

let partOne = dirSizes |> filter (fun s -> s <= 100000) |> sum
// part two

let totalAvailable = 70000000
let spaceRequired = 30000000
let unusedSpace = totalAvailable - (dirSizes |> maximum)
let spaceToFree = spaceRequired - unusedSpace
dirSizes |> sort |> find (fun size -> size >= spaceToFree)
