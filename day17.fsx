#r "nuget:FSharpPlus"
open FSharpPlus

let blocksString =
    """####

.#.
###
.#.

..#
..#
###

#
#
#
#

##
##"""

let toBlock (str: string) =
    let data = str.Split("\n") |> Array.rev
    let h = data.Length
    let w = data[0].Length
    Set [for x in 0..w - 1 do for y in 0..h - 1 do if data[y][x] = '#' then (x,y)]

let blocks = blocksString.Split("\n\n") |> Array.map toBlock

let displacement =
    //">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>" 
    System.IO.File.ReadAllText "17.txt"
    |> Seq.map (function '>' -> 1 | '<' -> -1 | _ -> failwith "wrong char")
    |> Seq.toArray

let hitWall block dx = block |> Set.exists (fun (x, _) -> x + dx < 0 || x + dx > 6)

let hitRocks rocks block dx dy =
    block |> Set.exists ( fun (x, y) -> rocks |> Set.contains (x + dx, y + dy))

// let endless s = seq { while true do yield! s }
let height rocks = rocks |> Seq.map snd |> Seq.max

let print rocks block bx by =
    let getchar y x  =
        if rocks |> Set.contains(x, y) then "#"
        elif block |> Set.contains (x - bx, y - by) then "@"
        else "."

    for y = height rocks + 5 downto 0 do
        let row = String.init 7 (getchar y)
        printfn $"|{row}|"
    printfn ""

let printFinal rocks =
    print rocks Set.empty 0 0

let rec drop rocks jetn bn  x y =
    // printfn $"drop: {y}"
    let block = blocks[bn]
    // print rocks block x y
    let dx = displacement[jetn]
    // printfn $"{dx} wall: {hitWall block (x + dx)} rocks: {hitRocks rocks block (x + dx) y}"
    let x = if hitWall block (x + dx) || hitRocks rocks block (x + dx) y then x else x + dx
    // print rocks block x y
    let nextJetn = (jetn + 1 ) % displacement.Length
    if hitRocks rocks block x (y - 1) then
        nextJetn,
        block |> Set.map (fun (bx, by) -> bx + x, by + y) |> Set
    else drop rocks nextJetn bn x (y - 1)

let compact rocks =
    let moves = [for x in [1; 0; -1] do for y in [1; 0; -1] do x, y]
    let rec findPath frindge (scores : Map<_,_>) =
        let next = 
            seq {
                for (vx, vy) as v in frindge do
                    for x, y in moves do
                        let next = (vx + x, vy + y)
                        let score = if scores[v] < vy + y then scores[v] else vy + y
                        match scores |> Map.tryFind next with
                        | Some sc when sc < score ->  yield next, score
                        | _ -> ()
            }
        let toVisit, scores = next |> map fst, (scores |> Map.union (next |> Map.ofSeq))
        if toVisit |> Seq.isEmpty then 0 else
            match toVisit |> Seq.filter (fun (x, y) -> x = 6) |> toList with
            | [] -> findPath toVisit scores
            | ps -> ps |> map (fun p -> scores[p]) |> maximum
        
    let (hx,hy) as highest = rocks |> Set.filter (fun (x, _) -> x = 0) |> Seq.maxBy snd
    if hy < 500 then
        rocks, 0
    else
        let lowest = findPath [ highest ] (rocks |> Set.map (fun p -> p, 0) |> Map.ofSeq |> Map.add highest hy)
        let compacted = rocks |> Set.filter (fun (_, y) -> y >= lowest) |> Set.map (fun (x, y) -> x, y - lowest)
        // printFinal compacted
        compacted, lowest

let rec dropN N =

    let scanner rocks jetn bn =
        let jetn, dropped = drop rocks jetn bn 2 (4 + height rocks)
        let rocks, h = compact (rocks + dropped)
        rocks, h, jetn

    let bottom = Set [for x in 0..6 -> x, 0]

    let rec step rocks totalH jetn = function
        | n when n = N ->
            (height rocks |> int64) + totalH
        | n ->
            let rocks, h, jetn = scanner rocks jetn (int (n % 5L))
            step rocks (int64 h + totalH) jetn (n + 1L)

    step bottom 0L 0 0

#time "on"
let x = [ for i in 1L..30L do i * 100L, dropN (i * 100L) ]



// compact rocks //|> printFinal








