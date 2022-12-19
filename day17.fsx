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

type CircularBuffer(bufferLength) =
    let a = Array.create bufferLength 0
    member _.Item
        with get (y: int64) = a[int (y % int64 bufferLength)]
        and set (y: int64) v = a[int (y % int64 bufferLength)] <- v

let jets =
    //">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>" 
    System.IO.File.ReadAllText "17.txt"
    |> Seq.map (function '>' -> 1 | '<' -> -1 | _ -> failwith "wrong char")
    |> Seq.toArray

let inline hitsWall dx segment =
    (segment >>> dx) &&& 0b10000000 = 0b10000000|| (segment >>> (dx - 1)) &&& 0b1 = 0b1
let inline hitsRocks rockLayer dx segment = rockLayer &&& (segment >>> dx) <> 0
let inline getSegment n block = (block >>> (n * 8)) &&& 0b11111111

let blocked (rocks: CircularBuffer) block dx y =
    [0..3] |> exists (fun n ->
        let segment = getSegment n block
        (hitsRocks (rocks[y + int64 n]) dx segment) || (hitsWall dx segment)
    )

let toRocks (rocks: CircularBuffer) block x y =
    for n in 0..3 do 
        let segment = getSegment n block
        rocks[y + int64 n] <- rocks[y + int64 n] ||| (segment >>> x)

let rec drop (rocks: CircularBuffer) jetn block x y =
    let dx = jets[jetn]
    let x = if blocked rocks block (x + dx) y then x else x + dx
    let nextJetn = (jetn + 1 ) % jets.Length
    if blocked rocks block x (y - 1L) then
        do toRocks rocks block x y
        nextJetn, y
    else drop rocks nextJetn block x (y - 1L)

let dropSeq buf =
    let rocks = CircularBuffer(buf)
    rocks[0] <- 127
    let rec findEmpty fromY =
        if rocks[fromY] = 0 then fromY else findEmpty (fromY + 1L)
    let rec step eY jetn (n: int64) =
        seq {
            for y in eY..eY + 7L do rocks[y] <- 0
            let jetn, nexty = drop rocks jetn blocks[n % 5L |> int] 3 (eY + 3L)
            let nexteY = (findEmpty nexty)
            yield nexteY - eY
            yield! step nexteY jetn (n + 1L)
        }
    step 1L 0 0L

#time "on"
let a = (dropSeq 64) |> take 2022 |> sum

