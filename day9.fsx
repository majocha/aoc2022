let input = 
    [
        for s in System.IO.File.ReadAllLines "9.txt" do
            s[0], s[2..] |> string |> int
    ] 

let moveSteps (move, n) =
    match move with 
        'R' -> 1, 0 | 'L' -> -1, 0 | 'U' -> 0, -1 | 'D' -> 0, 1 | _ -> failwith "error"
    |> List.replicate n

let headSteps = input |> List.collect moveSteps

let inline (++) (x, y) (dx, dy) = x + dx, y + dy

let headPositions = headSteps |> List.scan (++) (0, 0) 

let tailStep (hx, hy) (tx, ty) =
    let dx, dy = hx - tx, hy - ty
    let touches = abs dx < 2 && abs dy < 2
    if not touches then sign dx, sign dy else 0, 0

let tailStepFolder tail head = tail ++ (tailStep head tail)
 
let tailPositions headPositions = headPositions |> List.scan tailStepFolder (0, 0)

let countDistinct a = a |> List.distinct |> List.length

let partOne = headPositions |> tailPositions |> countDistinct

let partTwo = [1..9] |> List.fold (fun ps _ -> tailPositions ps) headPositions |> countDistinct