#r "nuget:FSharpPlus"
#time "on"
open FSharpPlus

type Resource = Ore | Clay | Obsidian | Geode
type Robot = Robot of producing: Resource
type Cost = (Resource * int) list
type Costs = Map<Robot, Cost>
type State = State of resources: Map<Resource, int> * robots: Map<Robot, int>

let parseBlueprint s =
    let i, a, b, c, d, e, f = 
        s |> sscanf "Blueprint %d: Each ore robot costs %d ore. Each clay robot costs %d ore. Each obsidian robot costs %d ore and %d clay. Each geode robot costs %d ore and %d obsidian."
    i,
    [
        Robot Ore, [Ore, a]
        Robot Clay, [Ore, b]
        Robot Obsidian, [Ore, c; Clay, d]
        Robot Geode, [Ore, e; Obsidian, f]
    ] |> Costs


let blueprints =
    System.IO.File.ReadAllLines "19.txt"
    |> map parseBlueprint

let search timeLimit (costs: Costs) =

    let maxCostFor =
        fun res ->
            costs.Values |> Seq.concat |> filter (fun (k, v) -> k = res) |> Seq.maxBy snd |> snd
        |> memoizeN

    let canBuild ((Robot res) as robot) (State (resources, robots)) =
        (res = Geode || maxCostFor res > robots[robot]) &&
        costs[robot] |> forall (fun (res, ammount) -> 
            resources[res] >= ammount)

    let build (Robot res as robot) (State (resources, robots)) =
        let robots = robots.Add(robot, robots[robot] + 1 )
        let rec pay resources = function
            | (res, price) :: rest -> 
                rest |> pay (resources |> Map.add res (resources[res] - price))
            | _ -> resources
        let resources = pay resources costs[robot]
        State (resources, robots)

    let boundary = [|0..timeLimit + 1|] |> map (fun t -> t * (t - 1) / 2)

    let score t (State (resources, robots)) =
        let geodeBots = robots[Robot Geode]
        let geodes = resources[Geode]
        geodes + geodeBots * t + boundary[t]

    let prune =
        let mutable best = 0
        fun t state ->
            let sc = score t state
            if sc > best && t = 0 then
                best <- sc
            sc < best

    let allRobots = Set [Robot Geode; Robot Obsidian; Robot Clay; Robot Ore] 
    
    let initialState = State (
            [Ore, 0; Clay, 0; Obsidian, 0 ; Geode, 0] |> Map,
            allRobots |> map (fun r -> r, 0) |> Map |> Map.add (Robot Ore) 1
        )

    let collected (State (resources, robots)) = 
        let resources = robots.Keys |> Seq.map (fun (Robot r as k) -> r, robots[k] + resources[r]) |> Map
        State (resources, robots)

    let rec allPaths t (goal: Robot) (state: State) =
        if prune t state then Seq.empty
        else
            seq {
                if t = 0 then yield state
                elif canBuild goal state then
                    let state' = build goal (collected state)
                    for robot in allRobots do yield! allPaths (t - 1) robot state'
                else
                    yield! allPaths (t - 1) goal (collected state)
            }

    let objective (State (resources, _))  = resources[Geode]

    seq { for goal in [Robot Clay; Robot Ore] do yield! allPaths timeLimit goal initialState } |> Seq.map objective |> maximum

let partOneProcess (i, costs) =

    let geodes = search 24 costs
    printfn $" Blueprint {i} done, {geodes} geodes."
    i * geodes

let partOne = blueprints |> Array.Parallel.map partOneProcess |> sum

let partTwoProcess (i, costs) = 
    search 32 costs |> tap (printfn "Blueprint %d. Found %d geodes." i)

let partTwo = blueprints[..2] |> Array.Parallel.map partTwoProcess |> Seq.reduce (*)