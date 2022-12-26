#r "nuget:FSharpPlus"
#time "on"
open FSharpPlus

type Resource = Ore | Clay | Obsidian | Geode

type Robot = Robot of producing: Resource

type Cost = Map<Resource, int>
type Costs = Map<Robot, Cost>

type Decision = Construct of Robot * constructedAt: int
type Decisions = Decision list

let parseBlueprint s =
    let i, a, b, c, d, e, f = 
        s |> sscanf "Blueprint %d: Each ore robot costs %d ore. Each clay robot costs %d ore. Each obsidian robot costs %d ore and %d clay. Each geode robot costs %d ore and %d obsidian."
    i,
    [
        Robot Ore, Cost [Ore, a]
        Robot Clay, Cost [Ore, b]
        Robot Obsidian, Cost [Ore, c; Clay, d]
        Robot Geode, Cost [Ore, e; Obsidian, f]
    ] |> Costs


let blueprints =
    System.IO.File.ReadAllLines "19.txt"
    |> map parseBlueprint

let search timeLimit (costs: Costs) =

    let resourceAfter time resource (decisions: Decisions) =
        seq {
            for Construct(Robot r, t) in decisions do
                if 0 < t  && t <= time then
                    - (costs[Robot r] |> Map.tryFind resource |> Option.defaultValue 0)
                if t < time && r = resource then
                    time - t
        } |> sum

    let canBuild robot decisions time =
        costs[robot]
        |> Map.toSeq |> forall (fun (res, ammount) -> 
            resourceAfter time res decisions >= ammount)

    let lastTime = function Construct (_ ,t) :: _ -> t | _ -> 0

    let score decisions =
        let scoreRobots robot w =
            (decisions |> filter (fun (Construct(r, _)) -> r = robot) |> length) * w
        let scoreResource res w =
            resourceAfter timeLimit res decisions * w
        seq {
            scoreRobots (Robot Ore) 1
            scoreRobots (Robot Clay) 10
            scoreRobots (Robot Obsidian) 100
            scoreRobots (Robot Geode) 1000
            scoreResource Ore 1
            scoreResource Clay 10
            scoreResource Obsidian 100
            scoreResource Geode 10_000
        } |> Seq.sum

    let prune =
        let best = Array.create (timeLimit + 1) 0
        fun decisions ->
            let t = lastTime decisions
            let sc = score decisions
            if sc > best[t] then
                best[t] <- sc
                // printfn $"best score: {sc} at time {t}"
                // printfn "%A" decisions
                // printfn ""
                false
            else
                let d = timeLimit - t
                sc < best[t] - d * 100

    let rec allPaths prune decisions =
        if prune decisions then Seq.empty else
        seq {
            for t in (lastTime decisions) .. timeLimit - 1 do
                for res in [Ore; Clay; Obsidian; Geode] do
                    let robot = Robot res
                    if canBuild robot decisions t then
                        let next = Construct (robot, t + 1) :: decisions
                        yield! allPaths prune next
                        if not (prune next) then
                            yield next
        }

    let initial = [Construct (Robot Ore, 0)]
    let objective = resourceAfter timeLimit Geode

    initial |> allPaths prune |> Seq.map objective |> maximum

let partOneProcess (i, costs) =
    printfn $"processing blueprint {i} ..."
    let geodes = search 24 costs
    printfn $" Blueprint {i} done, {geodes} geodes."
    i * geodes

// let partOne = blueprints |> Array.Parallel.map partOneProcess |> sum

// let partTwoProcess (_, costs) = search 32 costs

// let partTwo = blueprints[..2] |> Array.Parallel.map partTwoProcess |> sum

let _, b1 = blueprints[0]

search 32 b1