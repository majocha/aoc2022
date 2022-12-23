#r "nuget:FSharpPlus"
#time "on"
open FSharpPlus

let input = System.IO.File.ReadAllLines "23.txt"

let size = input |> length

let grove = [
    for x in 0..size - 1 do
        for y in 0 .. size - 1 do
            if input[y][x] = '#' then x, y
]

type D = N | NE | E | SE | S | SW | W | NW | Zero

let inline (++) (x, y) (dx, dy) = x + dx, y + dy

let adj p d =
    let a =
        match d with
        | NE -> -1, -1
        | N -> 0, -1
        | NW -> 1, 1
        | E -> 1, 0
        | W -> -1, 0
        | SE -> 1, 1
        | S -> 0, 1
        | SW -> -1, 1
        | Zero -> 0, 0
    a ++ p    

let elfInDir p elves d  = elves |> List.contains (adj p d)

let propose e elves =
    let (|Eid|_|) ds e = if ds |> exists (elfInDir e elves) then None  else Some ()
    match e with
    | Eid [N; NE; NW] -> N
    | Eid [S; SE; SW] -> S
    | Eid [W; NW; SW] -> W
    | Eid [E; NE; SW] -> E
    | _ -> Zero

let firstHalf elves = elves |> map (fun e -> e, propose e elves) 




