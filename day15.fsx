#r "nuget: FSharpPlus"

open FSharpPlus

let parseSensor s =
    let sx, sy, bx, by = sscanf "Sensor at x=%d, y=%d: closest beacon is at x=%d, y=%d" s
    (sx, sy), (bx, by)

let input = System.IO.File.ReadAllLines "15.txt" |> map parseSensor

let manh (sx, sy) (bx, by) = abs (sx - bx) + abs (sy - by)

let size = 4000000 

let rowCoverage n =
    let coverage = 
        [
            for (sx, _) as s, b in input do
                let d = manh s b - manh s (sx, n)
                if d >= 0 then sx - d, sx + d
        ] |> sort
    
    let rec findNotCovered (x1, x2) ranges =
        match ranges with
        | (x3, x4) :: rest ->
            if x2 < x3 then Some (x2 + 1)
            else findNotCovered (x1, [x2; x4] |> maximum) rest
        | [] -> if x2 < size then Some size else None
    
    findNotCovered (0, 0) coverage |> Option.map(fun x -> int64 x, int64 n)

#time "on"
let partTwo = 
    let x, y = seq {0..size} |> map rowCoverage |> find Option.isSome |> Option.get  
    x * int64 size + y
