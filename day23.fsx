#r "nuget:FSharpPlus"
#time "on"
open FSharpPlus

let input = System.IO.File.ReadAllLines "23.txt"

let sizeY = input |> length
let sizeX = input |> head |> length

type D = N | S | W | E
type Elf = int * int


let inline (=>) (x, y) (dx, dy) = x + dx, y + dy

let allAdj e =
    [
        for x in -1 .. 1 do
            for y in -1 .. 1 do
                if (x, y) <> (0, 0) then e => (x, y)
    ] |> Set

let adj e = function
    | N -> [for x in -1 .. 1 do e => (x, -1)]
    | S -> [for x in -1 .. 1 do e => (x, 1)]
    | W -> [for y in -1 .. 1 do e => (-1, y)]
    | E -> [for y in -1 .. 1 do e => (1, y)]

let move d e = adj e d |> item 1

let elves =
    [
        for x in 0..sizeX - 1 do
            for y in 0 .. sizeY - 1 do
                if input[y][x] = '#' then (x, y)
    ] |> Set


type Proposition = Proposition of Elf * Elf

let propose (elves: Set<Elf>) (dirs: D list) (e: Elf) =
    let good d = Set.intersect elves (Set (adj e d)) |> Set.isEmpty
    if elves |> Set.intersect (allAdj e) |> Set.isEmpty then
        Proposition (e, e)
    else
        dirs |> tryFind good 
        |> Option.map(fun d -> Proposition (e, move d e))
        |> Option.defaultValue (Proposition (e, e))

let firstHalf dirs elves = elves |> map (propose elves dirs)

let sameDest p1 p2 =
    let (Proposition(_, e1)) = p1
    let (Proposition(_, e2)) = p2
    e1 = e2

let secondHalf propositions =
    [|
        for (Proposition(prev, next) as p) in propositions do
            if propositions |> Set.remove p |> Set.exists (sameDest p) then
                prev
            else 
                next
    |] |> Set

let extent elves =
    let xs = elves |> map fst
    let ys = elves |> map snd
    xs |> minimum, ys |> minimum, xs |> maximum, ys |> maximum

let vis elves =
    use file = new System.IO.StreamWriter("vis23.txt")
    let x1, y1, x2, y2 = extent elves
    for y in y1 - 1 .. y2 + 1 do
        for x in x1 - 1 .. x2 + 1 do
            fprintf file "%c" (if elves |> Set.contains (x, y) then '#' else '.')
        fprintfn file  ""

let rec round n elves dirs =
    vis elves
    if n = 0 then elves else
    let next = elves |> firstHalf dirs |> secondHalf
    let dirs = (dirs |> skip 1) @ (dirs |> take 1)
    round (n - 1) next dirs

let countEmpty elves =
    let x1, y1, x2, y2 = extent elves
    [ 
        for y in y1 .. y2  do
            for x in x1 .. x2 do
                x, y
    ] |> List.filter ( fun p -> elves |> Set.contains p |> not ) |> length

round 10 elves [N; S; W; E] |> countEmpty


