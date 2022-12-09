let input = 
    [
        for s in System.IO.File.ReadAllLines "9.txt" do
            s[0], s[2..] |> string |> int
    ] 

let moveSteps (move, n) =
    match move with 
        'R' -> 1, 0 | 'L' -> -1, 0 | 'U' -> 0, -1 | 'D' -> 0, 1 | _ -> failwith "error"
    |> List.replicate n

let tailStep (hx, hy) (tx, ty) =
    let dx, dy = hx - tx, hy - ty
    let touch d = if abs d > 1 then sign d else 0
    if abs dx + abs dy > 2 then 
        //diagonal step
        sign dx, sign dy 
    else touch dx, touch dy

let headSteps = input |> List.collect moveSteps

let inline (++) (x, y) (dx, dy) = x + dx, y + dy

let stepFolder pos step = pos, pos ++ step

let foldWithLast f ps =
    let result, last = ps |> List.mapFold f (0, 0)
    result @ [last]

let headPositions = foldWithLast stepFolder headSteps

let tailStepFolder tail head = tail, tail ++ (tailStep head tail)

let countDistinct a = a |> List.distinct |> List.length 

let tailPositions headPositions = foldWithLast tailStepFolder headPositions

let partOne = headPositions |> tailPositions |> countDistinct

let partTwo = [1..9] |> List.fold (fun ps _ -> tailPositions ps) headPositions |> countDistinct
