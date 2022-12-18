#r "nuget:FSharpPlus"
#time "on"
open FSharpPlus

let parseLine = sscanf "%d,%d,%d"

let cubes = System.IO.File.ReadAllLines "18.txt" |> map parseLine |> Set

let moves = [
    1, 0, 0
    0, 1, 0
    -1, 0, 0
    0, -1, 0
    0, 0, -1
    0, 0, 1
]

let inline (+++) (x, y, z) (dx, dy, dz) = x + dx, y + dy, z + dz

let rec search cubes toVisit visited score = 

    let neighbours c =
        [
            for m in moves do
                let n = m +++ c
                if cubes |> Set.contains n then n
        ]

    let nextScore = score + (6 * Set.count toVisit) - (toVisit |> Seq.sumBy (neighbours >> length))
    let nextVisited = visited + toVisit
    let nextToVisit = (toVisit |> Seq.collect neighbours |> Set) - visited
    if nextToVisit.IsEmpty then nextScore else 
        search cubes nextToVisit nextVisited nextScore

let cmin f = cubes |> map f |> minimum
let cmax f = cubes |> map f |> maximum
let x1, x2 = cmin item1 - 1, cmax item1 + 1
let y1, y2 = cmin item2 - 1, cmax item2 + 1
let z1, z2 = cmin item3 - 1, cmax item3 + 1

let vat =
    [
        for x in x1..x2 do
        for y in y1..y2 do
        for z in z1..z2 do
            x, y, z
    ] |> Set

let partTwo = 
    search (vat - cubes) Set[x1, y1, z1] Set.empty 0 
  - search vat Set[x1, y1, z1] Set.empty 0