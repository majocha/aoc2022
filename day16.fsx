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
let visited path = path |> List.map fst |> Set 

let bestValves = valvesToOpen |> toSeq |> sortByDescending getFlow

type Agents = (string * int) list

let start remainingTime n =

    let good =
        let mutable best = 0

        fun agents -> function
        | (_, time) :: _ as path ->
            let vs = visited path
            let remaining = bestValves |> Seq.except vs
            let ts = 
                seq { for t in time - 1 .. -2 .. 0 do yield t; yield t }
            let q = ts |> zip remaining
            let score = totalFlow path + totalFlow q
            if (q |> Seq.isEmpty || agents |> length = 0) && score > best then
                printfn $"best {score} {path}"
                best <- score
                true
            else score > best
        | _ -> true
        // |> memoizeN

    let rec dfs (agents: Agents) = 
        function
        | path when good agents path ->
            let valvesLeft = valvesToOpen - (visited path)
            match agents with
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
                else steps |> Seq.collect (fun next -> dfs (next :: rest |> sortByDescending snd) (next :: path))
        | _ -> Seq.empty
        // |> memoizeN

    dfs [for i in 1..n do ("AA", remainingTime);] [] |> Seq.maxBy totalFlow

let partOne = start 30 1 |> totalFlow
let partTwo = start 26 2 |> totalFlow

