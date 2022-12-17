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
    [for x in 0..w - 1 do for y in 0..h - 1 do if data[y][x] = '#' then (x,y)]

let blocks = blocksString.Split("\n\n") |> Array.map toBlock

let displacement =
    ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>" //System.IO.File.ReadAllText "17.txt"
    |> Seq.scan (fun dx c -> if c = '>' then dx + 1 else dx - 1 ) 0

let hitBottom bottom block dy =
    block |> List.exists (fun (_, y) -> y + dy = 0)

let hitWall block dx = block |> List.exists (fun (x, _) -> x + dx < 0 || x + dx > 6)

let hitRocks rocks block dx dy =
    block |> List.exists ( fun (x, y) -> rocks |> Set.contains (x + dx, y + dy))

let sideMovements () =
    let loop = seq { while true do yield! displacement }
    let e = loop.GetEnumerator()
    fun () -> 
        e.MoveNext() |> ignore
        e.Current

let rec step next rocks block x y =
    let dx = next()
    let x = if hitWall block x + dx || hitRocks rocks block x + dx y then x else x + dx
    if hitBottom block y - 1 || hitRocks rocks block x y - 1 then
        blocks |> List.map (fun (bx, by) -> bx + x, by + y)
    else step






