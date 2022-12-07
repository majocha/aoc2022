#r "nuget: FSharpPlus, 1.2.5"
open FSharpPlus
let input = System.IO.File.ReadAllLines "7.txt" |> toList

type CdParam = Root | Up | Down of string
type Node = File of string * int | Dir of string * Node list


// parse the input for fun
type NodeInfo = FileInfo of int * string | DirName of string
type Command = Ls of NodeInfo list | Cd of CdParam

let cddir = function "/" -> Root | ".." -> Up | dir -> Down dir
let parseOne f = function | s :: rest -> Some rest |> Option.zip (f s) | _ -> None
let (|CdP|_|)  = parseOne ((trySscanf "$ cd %s") >> (Option.map cddir))
let (|LsP|_|) = function s :: rest when s = "$ ls" -> Some rest | _ -> None
let (|DirP|_|)  = parseOne (trySscanf "dir %s" >> Option.map DirName)
let (|FileP|_|) = parseOne (trySscanf "%d %s" >> Option.map FileInfo)

let rec (|Star|_|) f acc lines =
    match f lines with
    | Some (res, rest) ->
        (|Star|_|) f (res :: acc) rest
    | None ->
        (acc, lines) |> Some

let (|DirLine|_|) = function | DirP d -> Some d | FileP f -> Some f | _ -> None
let (|Command|_|) = function
    | CdP(param, rest) -> Some (Cd param, rest)
    | LsP(Star (|DirLine|_|) [] (dls, rest)) -> Some (Ls dls, rest)
    | _ -> None
let (|Input|_|) = function
    | Star (|Command|_|) [] commands -> Some commands
    | _ -> None

let parsed = match input with Input (cs,_) -> cs |> rev | _ -> []

let rec sumFileSizes nodes sum =
    match nodes with
    | [] -> sum
    | FileInfo(size, _) :: nodes -> sumFileSizes nodes (sum + size)
    | _ :: nodes -> sumFileSizes nodes sum

let rec replay commands result path =
    match commands with
    | [] -> result
    | (Cd param) :: commands ->
        match param with
        | Up -> replay commands result (path |> skip 1)
        | Root -> replay commands result ["/"]
        | Down dir -> replay commands result (dir :: path)
    | (Ls nodes) :: commands ->
        let pathStr = path |> rev |> String.intercalate "/"
        let result = (pathStr, sumFileSizes nodes 0) :: result 
        replay commands result path

let result = replay parsed [] []

let paths = result |> map fst
let partOne = 
    [ 
        for path in paths do
        let total =
            result
            |> List.filter (fun (p, _) -> p.StartsWith path)
            |> List.map snd
            |> sum
        if total <= 100_000 then yield total
    ] |> sum

// part two

let dirSizes = 
    [ 
        for path in paths do
            result
            |> filter (fun (p, _) -> p.StartsWith path)
            |> List.map snd
            |> sum
    ] |> List.sortByDescending id

let totalAvailable = 70000000
let spaceRequired = 30000000
let unusedSpace = totalAvailable - (dirSizes |> maximum)
let spaceToFree = spaceRequired - unusedSpace
dirSizes |> sort |> find (fun size -> size >= spaceToFree)
