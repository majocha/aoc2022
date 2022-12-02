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

let yourMoveScore = function Rock -> 1 | Paper -> 2 | Scissors -> 3
let OutcomeScore = function Win -> 6 | Lose -> 0 | Draw -> 3

let decideYourMove elfsMove outcome = 
    match outcome with
    | Win -> winningMoveFor elfsMove
    | Lose -> losingMoveFor elfsMove
    | Draw -> elfsMove 

let doRound (abc, xyz) =
    let elfsMove, outcome = parseMove abc, parseOutcome xyz
    let yourMove = decideYourMove elfsMove outcome
    yourMoveScore yourMove + OutcomeScore outcome

input |> map doRound |> sum