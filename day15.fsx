#r "nuget: FSharpPlus"
#time "on"

open FSharpPlus

let parseSensor s =
    let sx, sy, bx, by = sscanf "Sensor at x=%d, y=%d: closest beacon is at x=%d, y=%d" s
    (sx, sy), (bx, by)

let input = System.IO.File.ReadAllLines "15.txt" |> map parseSensor

let manh (sx, sy) (bx, by) = abs (sx - bx) + abs (sy - by)

let size = 4000000 

// fast solution, based on observation that the point we're looking for
// must be between some almost touching beacon circles (manhattan circles)
// if this doesn't give us a solution we should also check the corners of the area

let sensors = [ for s, b in input do s, manh s b ]

let potential = 
    [ 
        for (s1, r1), (s2, r2) in sensors |> List.allPairs sensors do
            if manh s1 s2 - r1 - r2 |> abs = 2 then
                s1, r1
    ]

let inArea (x, y) = x >= 0 && x <= size && y >= 0 && y <= size
let notCovered p = sensors |> forall (fun (s, r) -> manh s p > r)
let good p = inArea p && notCovered p

let circumference ((x, y), r) =
    seq {
        let r = r + 1
        for i in 0..r do
            x + i, y + r - i
            x + i, y - r + i
            x - r + i, y + i
            x - r + i, y - i
    }
    
let partTwoFast = 
   let x, y = potential |> Seq.collect circumference |> find good
   int64 x * int64 size + int64 y


// slower, but not a brute force solution

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

let partTwo = 
    let x, y = seq {0..size} |> map rowCoverage |> find Option.isSome |> Option.get  
    x * int64 size + y
