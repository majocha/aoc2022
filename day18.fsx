#r "nuget:FSharpPlus"
open FSharpPlus

let parseLine (str: string) =
    let a = str.Split(',') |> Array.map int
    a[0], a[1], a[2]

let cubes = System.IO.File.ReadAllLines "18.txt" |> Seq.map parseLine |> Set

let moves = [
    for x in [-1; 1] do
        for y in [-1; 1] do
            for z in [-1; 1] do
                x, y, z
]

let inline (+++) (x, y, z) (dx, dy, dz) = x + dx, y + dy, z + dz
type Cube = int * int * int
type Cubes = Set<Cube>


let rec search cubes toVisit visited score = 
    let next = 
        [
            for c in toVisit do
                let ns = 
                    [
                        for m in moves do
                            let n = m +++ c
                            if 
                                cubes - visited |> Set.contains n
                            then n
                    ]
                ns, 6 - ns.Length
        ]
    let visited = visited + toVisit
    let toVisit = (next |> List.collect fst |> Set) - toVisit
    printfn "next to visit: %A" toVisit
    if toVisit.IsEmpty then score else 
        let score = score + (next |> List.sumBy snd)
        search cubes toVisit visited score

let s = Set [1,1,1; 2,1,1]
let x = search s s Set.empty 0
