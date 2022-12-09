let input = 
    [
        for s in System.IO.File.ReadAllLines "9.txt" do
            s[0], s[2..] |> string |> int
    ]

let steps (move, n) =
    let step =
        match move with 
        'R' -> 1, 0 | 'L' -> -1, 0 | 'U' -> 0, -1 | 'D' -> 0, 1 | _ -> failwith "error"
    [ for _ in 1..n -> step ]

let tailStep (hx, hy) (tx, ty) =
    let dx, dy = hx - tx, hy - ty
    let sd d = if abs d > 1 then sign d else 0
    if abs dx + abs dy > 2 then
        // diagonal
        sign dx, sign dy
    else
        sd dx, sd dy

let headSteps = input |> List.collect steps
let doStep (x, y) (dx, dy) = x + dx, y + dy

let rec positions current result = function
    | [] -> current :: result |> List.rev
    | step :: rest ->
        let pos = doStep current step
        positions pos (current :: result) rest

let start = 0, 0
let headPositions = positions start [] headSteps

let rec tailPositions tail result = function
    | [] -> tail :: result |> List.rev
    | head :: rest ->
        let pos = doStep tail (tailStep head tail)
        tailPositions pos (tail :: result) rest

let countDistinct a = a |> List.distinct |> List.length 
let partOne = (tailPositions start [] headPositions) |> countDistinct

let rec next n result = function
    | prev when n = 0 -> countDistinct prev :: result
    |  prev ->
        let positions = tailPositions start [] prev
        next (n - 1) (countDistinct prev :: result) positions

let partTwo = next 9 [] headPositions |> List.head