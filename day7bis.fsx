// no tree solution

#r "nuget: FSharpPlus, 1.2.5"
open FSharpPlus

let input = System.IO.File.ReadAllLines "7.txt" |> toList

type CdParam = Root | Up | Down of string
let cddir = function "/" -> Root | ".." -> Up | dir -> Down dir
let (|Cd|_|)  = trySscanf "$ cd %s" >> Option.map cddir
let (|Ls|_|) = function "$ ls" -> Some () | _ -> None
let (|File|_|) = trySscanf "%d %s" >> Option.map fst

let rec sumFiles lines total =
    match lines with
    | [] -> total
    | (File size) :: rest -> sumFiles rest (total + size)
    | _ :: rest -> sumFiles rest total


let rec sumDirs input result path =
    match input with
    | [] -> result
    | line :: rest ->
        match line with
        | Cd Root -> sumDirs rest result ["/"]
        | Cd Up -> sumDirs rest result (path |> skip 1)
        | Cd (Down dir) -> sumDirs rest result (dir :: path)
        | Ls ->
            let lines = rest |> takeWhile (fun s -> not (s.StartsWith "$")) |> toList
            let total = sumFiles lines 0
            let pathStr = path |> rev |> String.intercalate "/"
            sumDirs (rest |> skip lines.Length) ((pathStr, total) :: result) path
        | _ -> failwith "wrong input" 

let result = sumDirs input [] []

let paths = result |> map fst

let partOne = 
    [ 
        for path in paths do
        let total =
            result
            |> filter (fun (p, _) -> p.StartsWith path)
            |> List.sumBy snd
        if total <= 100_000 then yield total
    ] |> sum

// part two

let dirSizes = 
    [ 
        for path in paths do
            result
            |> filter (fun (p, _) -> p.StartsWith path)
            |> List.sumBy snd
    ] |> sort |> rev

let totalAvailable = 70000000
let spaceRequired = 30000000
let unusedSpace = totalAvailable - (dirSizes |> maximum)
let spaceToFree = spaceRequired - unusedSpace
dirSizes |> sort |> find (fun size -> size >= spaceToFree)
