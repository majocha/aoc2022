#r "nuget:FSharpPlus"
#time "on"
open FSharpPlus

let input = System.IO.File.ReadAllLines "23.txt"

let sizeY = input |> length
let sizeX = input |> head |> length

type D = N | S | W | E
type Elf = int * int
type Proposition = Elf * Elf

let elves =
    [
        for x in 0..sizeX - 1 do
            for y in 0 .. sizeY - 1 do
                if input[y][x] = '#' then (x, y)
    ] |> Set

let inline (=>) (x, y) (dx, dy) = x + dx, y + dy

let allAdj e =
    [
        for x in -1 .. 1 do
            for y in -1 .. 1 do
                if (x, y) <> (0, 0) then e => (x, y)
    ]

let adj e = function
    | N -> [for x in -1 .. 1 do e => (x, -1)]
    | S -> [for x in -1 .. 1 do e => (x, 1)]
    | W -> [for y in -1 .. 1 do e => (-1, y)]
    | E -> [for y in -1 .. 1 do e => (1, y)]

let move d e = adj e d |> item 1

let propose (elves: Set<Elf>) (dirs: D list) (e: Elf) =
    let good d = (adj e d) |> forall (fun p -> elves |> Set.contains p |> not)
    if allAdj e |> exists (fun p -> elves |> Set.contains p) then
        dirs |> tryFind good 
        |> Option.map(fun d -> Proposition (e, move d e))
        |> Option.defaultValue (Proposition (e, e))
    else Proposition (e, e)

let firstHalf dirs elves = elves |> map (propose elves dirs)

let secondHalf (ps: Set<Proposition>) = 
    let bad = 
        ps |> groupBy snd |> map (fun (d, ps) -> d, ps |> length)
        |> filter ( fun (d, l) -> l > 1) |> map fst |> Set
    ps |> Set.map ( fun (current, next) -> if bad |> Set.contains next then current else next)

module PartOne =

    let rec round n elves dirs =
        if n = 0 then elves else
            let next = elves |> firstHalf dirs |> secondHalf
            let dirs = (dirs |> skip 1) @ (dirs |> take 1)
            round (n - 1) next dirs

    let countEmpty elves =
        let xs = elves |> map fst
        let ys = elves |> map snd
        let x1, y1, x2, y2 = xs |> minimum, ys |> minimum, xs |> maximum, ys |> maximum
        [ 
            for y in y1 .. y2  do
                for x in x1 .. x2 do
                    x, y
        ] |> List.filter ( fun p -> elves |> Set.contains p |> not ) |> length

    let partOne = round 10 elves [N; S; W; E] |> countEmpty


module PartTwo =

    let rec round n elves dirs =
        let next = elves |> firstHalf dirs |> secondHalf
        if next = elves then n else
            let dirs = (dirs |> skip 1) @ (dirs |> take 1)
            round (n + 1) next dirs

    let partTwo = round 1 elves [N; S; W; E]
