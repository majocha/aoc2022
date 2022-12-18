#r "nuget:FSharpPlus"
#time "on"
open FSharpPlus

let parseLine (str: string) =
    let a = str.Split(',') |> Array.map int
    a[0], a[1], a[2]

let cubes = System.IO.File.ReadAllLines "18.txt" |> Seq.map parseLine |> Set

let moves = [
    1, 0, 0
    0, 1, 0
    -1, 0, 0
    0, -1, 0
    0, 0, -1
    0, 0, 1
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
                            if cubes |> Set.contains n then n
                    ]
                c, ns
        ]
    let scoreAfterVisit = next |> List.map (fun (c, ns) -> c, 6 - length ns) |> Map
    let score = score |> Map.union scoreAfterVisit
    let neighbours = next |> List.collect snd |> Set
    let toVisit, visited = neighbours - visited, visited + toVisit
    if toVisit.IsEmpty then score |> Map.values |> sum else 
        search cubes toVisit visited score

let test = [1,1,1; 2,1,1; 3, 1, 1; 10, 10, 10] |> Set
search test Set[10, 10, 10] Set.empty Map.empty

cubes |> Seq.maxBy item1
cubes |> Set.minElement

let vat =
    [
        for x in -1..22 do
        for y in -1..22 do
        for z in -1..22 do
            x, y, z
    ] |> Set

search (vat - cubes) Set[0, 0, 0] Set.empty Map.empty 
5900 - 3456
3456