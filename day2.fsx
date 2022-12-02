#r "nuget: FSharpPlus, 1.2.5"
open FSharpPlus

let input = System.IO.File.ReadAllLines "2.txt" |> map (sscanf "%s %s")

type Move = Rock | Paper | Scissors
type Outcome = Win | Lose | Draw

let parseMove = function
    | "A"  -> Rock | "B" -> Paper | "C"  -> Scissors
    | _ -> failwith "wrong input"

let parseOutcome = function
    | "X" -> Lose | "Y" -> Draw | "Z" -> Win
    | _ -> failwith "wrong input"

let winningMoveFor = function
    | Rock -> Paper | Scissors -> Rock | Paper -> Scissors

let losingMoveFor = function
    | Rock -> Scissors | Scissors -> Paper | Paper -> Rock

let playScore elfsMove yourMove =
    let yourSelection =
        match yourMove with
        | Rock -> 1 | Paper -> 2 | Scissors -> 3
    let outcome =
        if elfsMove = yourMove then 3
        elif winningMoveFor elfsMove = yourMove then 6
        else 0
    outcome + yourSelection

let decideYourMove elfsMove outcome = 
    match outcome with
    | Win -> winningMoveFor elfsMove
    | Lose -> losingMoveFor elfsMove
    | Draw -> elfsMove 

let doRound (abc, xyz) =
    let elfsMove, outcome = parseMove abc, parseOutcome xyz
    let yourMove = decideYourMove elfsMove outcome
    playScore elfsMove yourMove

input |> map doRound |> sum