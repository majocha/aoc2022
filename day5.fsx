#r "nuget: FSharpPlus, 1.2.5"
open FSharpPlus
open System

let input = IO.File.ReadAllLines "5.txt"
let height = Array.IndexOf(input, "")
let columnIndexes  =
    input[height-1] |> Seq.indexed |> filter (snd >> Char.IsAsciiDigit) |> toList 

let parseStack x =
    [ for i in 0..height - 2 -> input[i][x] ]
    |> filter Char.IsAsciiLetter
let stacks = [for i, c in columnIndexes do c, parseStack i] |> Map

let moves = input[height+1..] |> map (sscanf "move %d from %c to %c") |> toList

let move source dest n =
    source |> skip n, (source |> take n(* |> rev *)) @ dest

let step (stacks: Map<_,_>) (n, si, di) =
    let source, dest = move stacks[si] stacks[di] n
    stacks
    |> Map.add si source
    |> Map.add di dest

moves |> fold step stacks |> Map.values |> map head |> String.ofSeq
