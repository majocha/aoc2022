#r "nuget: FSharpPlus"
#time "on"

open FSharpPlus
open System.Text.RegularExpressions
type Valve = { Name: string; Flow: int; Connections: string list }
let parseList s = s |> String.split [ ", " ] |> toList
let parseLine s = 
    let vs = [ for g in Regex.Match(s, @"Valve (\w+) has flow rate=(\d+); (?:tunnels|tunnel) (?:leads|lead) to (?:valves|valve) (.*)").Groups -> g.Value ]
    { Name = vs[1]; Flow = int vs[2]; Connections = parseList vs[3] }

let valves = System.IO.File.ReadAllLines "16.txt" |> map parseLine
let valvesToOpen = valves |> filter (fun v -> v.Flow > 0) |> map (fun v -> v.Name) |> Set

let getFlow =
    let vs = [for v in valves -> v.Name, v.Flow] |> Map
    fun n -> vs[n]

let getConnections =
    let vs = [for v in valves -> v.Name, v.Connections] |> Map
    fun n -> vs[n]

let totalFlow path = path |> map (fun (valve, openingTime) -> openingTime * getFlow valve) |> sum

let rec bestPath1 =
    fun start valvesToOpen time ->
        if time = 0 || valvesToOpen |> Set.isEmpty then []
        else
            seq {

                if valvesToOpen |> Set.contains start then
                        yield (start, time) :: bestPath1 start (valvesToOpen |> Set.remove start) (time - 1)

                for v in getConnections start do
                        yield bestPath1 v valvesToOpen (time - 1)

            } |> Seq.maxBy totalFlow 
    |> memoizeN

let partOne = bestPath1 "AA" valvesToOpen 29 |> totalFlow


// let rec bestPath =
//     fun p1 p2 valvesToOpen time ->
//         if time = 0 || valvesToOpen |> Set.isEmpty then []
//         else
//             seq {

//                 let both = Set [p1; p2]
//                 if p1 <> p2 && both.IsSubsetOf(valvesToOpen) then
//                     yield (p2, time) :: (p1, time) :: bestPath p1 p2 (valvesToOpen - both) (time - 1)

//                 if valvesToOpen |> Set.contains p2 then
//                     for v1 in getConnections p1 do
//                         yield (p2, time) :: bestPath v1 p2 (valvesToOpen |> Set.remove p2) (time - 1)

//                 if valvesToOpen |> Set.contains p1 then
//                     for v2 in getConnections p2 do
//                         yield (p1, time) :: bestPath p1 v2 (valvesToOpen |> Set.remove p1) (time - 1)

//                 for v1 in getConnections p1 do
//                     for v2 in getConnections p2 do
//                         yield bestPath v1 v2 valvesToOpen (time - 1)

//             } |> Seq.filter (fun p -> totalFlow p < 1790) |> Seq.maxBy totalFlow 
//     |> memoizeN


// let partTwo = bestPath "AA" "AA" valvesToOpen 25 |> totalFlow

let rec bestPath =
    fun p1 p2 valvesToOpen time ->
        if time = 0 || valvesToOpen |> Set.isEmpty then []
        else
            seq {

                let both = Set [p1; p2]
                if p1 <> p2 && both.IsSubsetOf(valvesToOpen) then
                    yield (p2, time) :: (p1, time) :: bestPath p1 p2 (valvesToOpen - both) (time - 1)

                if valvesToOpen |> Set.contains p2 then
                    for v1 in getConnections p1 do
                        yield (p2, time) :: bestPath v1 p2 (valvesToOpen |> Set.remove p2) (time - 1)

                if valvesToOpen |> Set.contains p1 then
                    for v2 in getConnections p2 do
                        yield (p1, time) :: bestPath p1 v2 (valvesToOpen |> Set.remove p1) (time - 1)

                for v1 in getConnections p1 do
                    for v2 in getConnections p2 do
                        yield bestPath v1 v2 valvesToOpen (time - 1)

            } |> Seq.filter (fun p -> totalFlow p < 1790) |> Seq.maxBy totalFlow 
    |> memoizeN


let partTwo = bestPath "AA" "AA" valvesToOpen 25 //|> totalFlow
