#r "nuget:FSharpPlus"
#time "on"
open FSharpPlus

// let input = System.IO.File.ReadAllLines "19.txt"

type Resource = Ore | Clay | Obsidian | Geode

type Robot = Robot of producing: Resource

type Cost = Map<Resource, int>
type Costs = Map<Robot, Cost>

type Decision = Construct of Robot * constructedAt: int
type Decisions = Decision list

let costs =
    [
        Robot Ore, Cost [Ore, 4]
        Robot Clay, Cost [Ore, 2]
        Robot Obsidian, Cost [Ore, 3; Clay, 14]
        Robot Geode, Cost [Ore, 2; Obsidian, 7]
    ] |> Costs

let resourceAfter time resource (costs: Costs) (decisions: Decisions) =
    seq {
        for Construct(Robot r, t) in decisions do
            if 0 < t  && t <= time then
                - (costs[Robot r] |> Map.tryFind resource |> Option.defaultValue 0)
            if t < time && r = resource then
                time - t
    } |> sum

let canBuild robot decisions time (costs: Costs) =
    costs[robot]
    |> Map.toSeq |> forall (fun (res, ammount) -> 
        resourceAfter time res costs decisions >= ammount)

let score timeLimit decisions  =
    let scoreRobots robot w =
        (decisions |> filter (fun (Construct(r, _)) -> r = robot) |> length) * w
    let scoreResource res w =
        resourceAfter timeLimit res costs decisions * w

    seq {
        scoreRobots (Robot Ore) 1
        scoreRobots (Robot Clay) 5
        scoreRobots (Robot Obsidian) 100
        scoreRobots (Robot Geode) 1000
        // scoreResource Ore 10
        // scoreResource Clay 100
        // scoreResource Obsidian 1000
        scoreResource Geode 1_000_000
    } |> Seq.sum

let nextDecision decisions fromTime timeLimit =
    seq {
        for t in fromTime .. timeLimit - 1 do
            for res in [Ore; Clay; Obsidian; Geode] do
                let robot = Robot res
                if canBuild robot decisions t costs then
                    Construct (robot, t + 1)
    }

let rec loopSearch decisions timeLimit (best: int array) =
    let (Construct(_, fromTime)) = decisions |> head 
    seq {
        yield decisions
        for Construct (_, t) as d in nextDecision decisions (fromTime + 1) timeLimit do
            let s = score timeLimit (d :: decisions)
            if s > best[t] then best[t] <- s
            if s > best[t] - 1000 then
                yield loopSearch (d :: decisions) timeLimit best
    } |> Seq.maxBy (resourceAfter timeLimit Geode costs)

let initial = [Construct (Robot Ore, 0)]

let search timeLimit =
    let best = Array.create (timeLimit + 1) 0
    loopSearch initial timeLimit best |> rev

let ds = search 24
ds |> resourceAfter 24 Geode costs

canBuild (Robot Clay) initial 1 costs //|> resourceAt 25 Geode costs
resourceAfter 1 Ore costs initial
