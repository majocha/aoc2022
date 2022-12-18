#r "nuget:FSharpPlus"
open FSharpPlus

let blocks = 
    let toInt (data: int array) =
        [0..3] |> List.fold (fun b i -> (b <<< 8) ^^^ data[i]) 0
        <<< 4

    [|
        [|
            0
            0
            0  
            0b1111
        |]

        [|
            0
            0b0100
            0b1110
            0b0100
        |]

        [|
            0
            0b0010
            0b0010
            0b1110
        |]

        [|
            0b1000
            0b1000
            0b1000
            0b1000
        |]

        [|
            0
            0
            0b1100
            0b1100
        |]
    |] |> map toInt

blocks |> iter (printfn "%032B")

let displacement =
    //">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>" 
    System.IO.File.ReadAllText "17.txt"
    |> Seq.map (function '>' -> 1 | '<' -> -1 | _ -> failwith "wrong char")
    |> Seq.toArray

let inline hitsWall dx segment =
    (segment >>> dx) &&& 0b10000000 = 0b10000000|| (segment >>> (dx - 1)) &&& 0b1 = 0b1
let inline hitsRocks rockLayer dx segment = rockLayer &&& (segment >>> dx) <> 0
let inline getSegment n block = (block >>> (n * 8)) &&& 0b11111111

let print (rocks: int array) h =
    for y = h to rocks.Length - 1 do
        [ for c in sprintf "|%07B|" rocks[y] do
            match c with '0' -> '.' | '1' -> '#' | c -> c ]
        |> String.ofList |> printfn "%s"
    printfn ""

let blocked (rocks: int array) block dx y =
    [0..3] |> exists (fun n ->
        let segment = getSegment n block
        (hitsRocks rocks[y - n] dx segment) || (hitsWall dx segment)
    )

let toRocks (rocks: int array) block x y =
    for n in 0..3 do 
        let segment = getSegment n block
        rocks[y - n] <- rocks[y - n] ||| (segment >>> x)

let rec drop rocks jetn block x y =
    let dx = displacement[jetn]
    let x = if blocked rocks block (x + dx) y then x else x + dx
    let nextJetn = (jetn + 1 ) % displacement.Length
    if blocked rocks block x (y + 1) then
        do toRocks rocks block x y
        nextJetn, y
    else drop rocks nextJetn block x (y + 1)

// let compact rocks =
//     let moves = [for x in [1; 0; -1] do for y in [1; 0; -1] do x, y]
//     let rec findPath frindge (scores : Map<_,_>) =
//         let next = 
//             seq {
//                 for (vx, vy) as v in frindge do
//                     for x, y in moves do
//                         let next = (vx + x, vy + y)
//                         let score = if scores[v] < vy + y then scores[v] else vy + y
//                         match scores |> Map.tryFind next with
//                         | Some sc when sc < score ->  yield next, score
//                         | _ -> ()
//             }
//         let toVisit, scores = next |> map fst, (scores |> Map.union (next |> Map.ofSeq))
//         if toVisit |> Seq.isEmpty then 0 else
//             match toVisit |> Seq.filter (fun (x, y) -> x = 6) |> toList with
//             | [] -> findPath toVisit scores
//             | ps -> ps |> map (fun p -> scores[p]) |> maximum
        
//     let (hx,hy) as highest = rocks |> Set.filter (fun (x, _) -> x = 0) |> Seq.maxBy snd
//     if hy < 500 then
//         rocks, 0
//     else
//         let lowest = findPath [ highest ] (rocks |> Set.map (fun p -> p, 0) |> Map.ofSeq |> Map.add highest hy)
//         let compacted = rocks |> Set.filter (fun (_, y) -> y >= lowest) |> Set.map (fun (x, y) -> x, y - lowest)
//         // printFinal compacted
//         compacted, lowest

let dropN N =
    let rocks = Array.zeroCreate 4000
    rocks[3999] <- 127
    let rec findZero fromY =
        if rocks[fromY] = 0 then fromY else findZero (fromY - 1)

    let rec step zeroY jetn n =
        if n = N then 3998 - findZero 3999
        else
            let jetn, y = drop rocks jetn blocks[n % 5] 3 (zeroY - 3)
            // print rocks ((findZero y) - 3)
            step (findZero y) jetn (n + 1)

    step 3998 0 0

#time "on"
dropN 2022
