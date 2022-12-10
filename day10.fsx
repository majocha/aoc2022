type Instr = Addx of int | Noop
let input =
    System.IO.File.ReadAllLines "10.txt" 
    |> Array.map (fun s -> 
        match s[..3] with
        | "addx" -> s[4..] |> string |> int |> Addx
        | "noop" -> Noop
        | _ -> failwith "error"
    )

let cycles =
    [|
        let mutable X = 1
        for i in input do
            match i with
            | Noop -> 
                yield X
            | Addx v ->
                yield X
                yield X
                X <- X + v
    |]

let partOne =
    [20; 60; 100; 140; 180; 220] |> List.sumBy (fun n -> n * cycles[n - 1])

for y in 0..5 do
    for x in 0..39 do
        let center = cycles[y * 40 + x]
        if x >= center - 1 && x <= center + 1 then '#' else '.'
        |> printf "%c"
    printfn ""