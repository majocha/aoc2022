#r "nuget: FSharpPlus"
#time "on"

open FSharpPlus
open System.Text.RegularExpressions

let parseList s = s |> String.split [ ", " ] |> toList
let parseLine s = 
    let vs = [ for g in Regex.Match(s, @"Valve (\w+) has flow rate=(\d+); (?:tunnels|tunnel) (?:leads|lead) to (?:valves|valve) (.*)").Groups -> g.Value ]
    vs[1], (int vs[2], parseList vs[3])

let valves = System.IO.File.ReadAllLines "16.txt" |> map parseLine |> Map

let flow v = valves[v] |> fst

let connections v = valves[v] |> snd

type Action = Open of string * int | Move of string * int

let scoreAction = function Open (v, t) -> flow v * t | _ -> 0

let getGScore opens = opens |> Map.values |> Seq.sumBy scoreAction

let nextActions a =
    [
        match a with
        | Move (v, t)  when t > 0 ->
            yield Open (v, t - 1)
            yield! connections v |> map (fun v -> Move (v, t - 1))
        | Open (v, t) when t > 0 ->
            yield! connections v |> map (fun v -> Move (v, t - 2))
        | _ -> ()
    ]

let rec search opens = function 
    | [] -> opens
    | actions ->
        let mutable opens = opens
        let next = [
            for a in actions |> sortByDescending scoreAction do
                for n in nextActions a do
                        match n with 
                        | Open (v, t) as o ->
                            if opens |> Map.add v o |> getGScore > getGScore opens then
                                // need to record previous
                                opens <- opens |> Map.add v o
                                yield n
                        | Move _ ->
                            if opens |> length < 6 then yield n
        ]
        search opens next 

let start () =
    let o = Open("AA", 30)
    let opens = ["AA", o] |> Map
    search opens [o]

start ()

valves.Values |> filter (fun v -> fst v > 0) |> length


