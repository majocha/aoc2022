#r "nuget: FSharpPlus"
#time "on"

open FSharpPlus
open System.Text.RegularExpressions

let parseList s = s |> String.split [ ", " ]
let parseLine s = 
    let vs = [ for g in Regex.Match(s, @"Valve (\w+) has flow rate=(\d+); (?:tunnels|tunnel) (?:leads|lead) to (?:valves|valve) (.*)").Groups -> g.Value ]
    vs[1], int vs[2], parseList vs[3]

let input = System.IO.File.ReadAllLines "16.txt" |> map parseLine

let flows = Map [ for v, f, _ in input do v, f ]
let connections = Map [ for v, _, vs in input do v, vs ]
type Action = Open of string | Move of string
let score actions = actions |> List.indexed |> List.fold (fun total (i, a) -> match a with Open v -> flows[v] * i + total | _ -> total) 0

let rec search =
    fun actions t ->
        match t with 
        | 1 -> actions
        | t ->
            seq {
                let t = t - 1
                let current = match head actions with | Open v -> v | Move v -> v
                if not (actions |> List.contains (Open current)) then
                    yield search (Open current :: actions) t 
                for v in connections[current] do
                    yield search (Move v :: actions) t

            } |> maxBy score
    |> memoizeN

[
    for i in 1..12 do
        [ for v in connections.Keys do search [Move v] i ] |> maximum
]

