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
    let vs = [ for v in valves -> v.Name, v.Flow ] |> Map
    fun n -> vs[n]

let getConnections =
    let vs = [ for v in valves -> v.Name, v.Connections ] |> Map
    fun n -> vs[n] |> sortByDescending getFlow
    |> memoizeN

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

let distance =
    fun v1 ->
        let distances = dijkstra Map.empty [v1, 0]
        fun v2 -> distances[v2]
    |> memoizeN

let totalFlow path = path |> Seq.map (fun (valve, openingTime) -> openingTime * getFlow valve) |> sum
let visited path = path |> Seq.map fst |> Set 

let bestValves = valvesToOpen |> toSeq |> sortByDescending getFlow

let start remainingTime n =

    let good =
        let mutable best = 0
        let update score = if score > best then best <- score

        fun path -> function
            | [] -> totalFlow path |> tap update = best       
            | agents ->
                let remaining = bestValves |> Seq.except (visited path)
                let q = 
                    seq { 
                        for (_, time) in agents do
                        for t in 0 .. 2 .. time - 1 do t
                    } |> sort |> rev |> zip remaining
                totalFlow path + totalFlow q > best

    let rec dfs agents = function
        | path when good path agents ->
            let valvesLeft = valvesToOpen - (visited path)
            match agents |> sortByDescending snd with
            | [] -> seq { path }
            | (goal, t) :: rest -> 
                let steps =
                    seq {
                        if t = 0 then goal, t else
                        for next in valvesLeft |> sortByDescending (distance goal) do
                            let time = t - distance goal next - 1
                            if time > 0 then next, time
                    }
                if steps |> Seq.isEmpty then dfs rest path
                else steps |> Seq.collect (fun next -> dfs (next :: rest) (next :: path))
        | _ -> Seq.empty

    dfs [ for i in 1..n do ("AA", remainingTime) ] [] |> Seq.maxBy totalFlow

let partOne = start 30 1 |> totalFlow

let partTwo = start 26 2 |> totalFlow
