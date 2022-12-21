#r "nuget:FSharpPlus"
#time "on"
open FSharpPlus

let input = System.IO.File.ReadAllLines "21.txt"

type Op = Add | Sub | Div | Mult
and Yell = Number of int64 | Operation of Op * Yell * Yell | Human | Root of Yell * Yell

let (|OPERATION|_|) = trySscanf "%s %s %s"
let (|NUMBER|_|) = trySscanf "%d"
let (|OP|_|) = function
    "+" -> Some Add | "-" -> Some Sub | "/" -> Some Div | "*" -> Some Mult | _ -> None
let parseLine s = sscanf "%s: %s" s

let lines = input |> Seq.map parseLine |> Map

module PartOne =
    let rec parseOp name =
        match lines[name] with
        | NUMBER x -> Number x
        | OPERATION (n1, OP op, n2) -> Operation (op, parseOp n1, parseOp n2)
        | _ -> failwith "error"

    let tree = parseOp "root"

    let calcOp op a b =
        match op with Add -> a + b | Sub -> a - b | Mult -> a * b | Div -> a / b

    let rec calc = function 
        | Number d -> d 
        | Operation (op, p1, p2) -> calcOp op (calc p1) (calc p2)

    let partOne = calc tree


module PartTwo =
    let rec parseOp name =
        match lines[name] with
        | _ when name = "humn" -> Human
        | NUMBER x -> Number x
        | OPERATION (n1, OP op, n2) when name = "root" -> Root (parseOp n1, parseOp n2)
        | OPERATION (n1, OP op, n2) -> Operation (op, parseOp n1, parseOp n2)
        | _ -> failwith "error"

    let calcOp op a b =
        match op with Add -> a + b | Sub -> a - b | Mult -> a * b | Div -> a / b

    let rec calc human = function
        | Number d -> d
        | Human -> human
        | Operation (op, p1, p2) -> calcOp op (calc human p1) (calc human p2)
        | _ -> failwith "error"

    let (Root (p1, p2)) = parseOp "root"
    let constantBranch = calc 0L p2

    let calcH human = abs (constantBranch - calc human p1) 

    let rand = System.Random()
    let rec shoot best besth count =
        if count > 50_000 then besth else
            let r = rand.NextInt64(besth - best - 5000L, besth + best + 5000L)
            let c = calcH r
            if c < best then
                printfn $"h: {r} dist: {c}"
                shoot c r (count + 1)
            else shoot best besth (count + 1)

    let approx = shoot constantBranch constantBranch 0
    let partTwo = 
    [| 
        for i in approx - 5000L .. approx + 5000L do
        let c = calcH i
        if c = 0 then i
    |] |> minimum

