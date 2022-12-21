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
    |] |> Array.map toInt

let inline circular length i =
    if i >= 0L then i % length else length + i % length - 1L
    |> int

type CircularBuffer(length) =
    let a = Array.create length 0uy
    member _.Item with get i = a[circular length i] and set i v = a[circular length i] <- v

let jets =
    System.IO.File.ReadAllText "17.txt"
    |> Seq.map (function '>' -> 1 | '<' -> -1 | _ -> failwith "wrong char")
    |> Seq.toArray

let inline hitsWall dx segment =
    (segment >>> dx) &&& 0b10000000uy = 0b10000000uy|| (segment >>> (dx - 1)) &&& 0b1uy = 0b1uy

let inline hitsRocks rockLayer dx segment = rockLayer &&& (segment >>> dx) <> 0uy

let inline getSegment n block = (block >>> (n * 8)) |> byte

let blocked (rocks: CircularBuffer) block dx y =
    [0..3] |> List.exists (fun n ->
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
        nextJetn
    else drop rocks nextJetn block x (y - 1L)

let dropSeq (jetn, layers) y startN takeEvery =
    let rocks = CircularBuffer(512)
    for i = 0 to (layers |> Array.length) - 1 do
        rocks[y - int64 i] <- layers[i]

    let rec findHeight startH =
        if rocks[startH] = 0uy then startH else findHeight (startH + 1L)

    let rec step y jetn (n: int64) =
        seq {
            let height = findHeight y
            let bn = n % 5L |> int
            if n % takeEvery = 0L  then
                let layers = [| for i in height - 8L .. height - 1L do rocks[i] |]
                yield (jetn, layers), (n, height - 1L)
            for y in height..height + 7L do rocks[y] <- 0uy
            let nextJetn = drop rocks jetn blocks[bn] 3 (height + 3L)
            yield! step (findHeight height) nextJetn (n + 1L)
        }

    step y jetn startN

#time "on"
let result = Seq.take 1 >> Seq.map snd >> Seq.head

let partOne = (dropSeq (0, [| 127uy |]) 0L 0L 2022L) |> Seq.skip 1 |> result

let zeros = (dropSeq (0, [| 127uy |]) 0L 0L 5L) |> Seq.take 100_000 |> Seq.toList
let magic = zeros |> Seq.countBy fst |> Seq.sortBy snd |> Seq.find (fun (j, c) -> c > 1) |> fst
let repeating f = zeros |> Seq.filter (fun (p, _) -> p = magic) |> Seq.map snd |> Seq.map f |> Seq.take 3 |> Seq.toArray
let ns, ys = repeating fst, repeating snd
let startsAtN, repeatsEveryN = ns[0], ns[2] - ns[1]
let startsAt, repeatsEvery = ys[0], ys[2] - ys[1]

let cycles = (1000000000000L - startsAtN) / repeatsEveryN
let y = startsAt + cycles * repeatsEvery
let n = startsAtN + cycles * repeatsEveryN

let partTwo = dropSeq magic y n 1000000000000L |> result
