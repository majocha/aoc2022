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
let getConnections n = (valves |> find (fun v -> v.Name = n)).Connections
let valvesToOpen = valves |> filter (fun v -> v.Flow > 0) |> map (fun v -> v.Name) |> Set
let getFlow =
    let vs = [for v in valves -> v.Name, v.Flow] |> Map
    fun n -> vs[n]

let rec dijkstra dist = function
    | [] -> dist
    | toVisit ->
        let dist = dist |> Map.union (Map toVisit)
        [
            for n, d in toVisit do
                for c in getConnections n do
                    match dist |> Map.tryFind c with
                    | Some d1 when d + 1 < d1 ->
                        yield c, d + 1
                    | None -> yield c, d + 1
                    | _ -> ()
        ] |> dijkstra dist

let distances = dijkstra Map.empty ["AA", 0]

let distance v1 v2 = abs (distances[v1] - distances[v2])

let totalFlow path = [ for valve, openingTime in path -> openingTime * getFlow valve] |> sum

let rec bestPath =
    fun start valvesToOpen remainingTime ->
        let potential =
            seq {
                for v in valvesToOpen do
                    let time = remainingTime - 1 - distance start v
                    if time >= 0 then
                        (start, remainingTime) :: bestPath v (valvesToOpen |> Set.remove v) (time)
                    else 
                        [start, remainingTime]
            } 
        if potential |> Seq.isEmpty then []
        else potential |> Seq.maxBy totalFlow 
    |> memoizeN

let x = bestPath "AA" (valvesToOpen |> Set.remove "AA") 29

totalFlow x