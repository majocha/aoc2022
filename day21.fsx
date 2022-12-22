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
        | Root (p1, p2) -> (calc human p1) - (calc human p2) |> abs
        | Operation (op, p1, p2) -> calcOp op (calc human p1) (calc human p2)

    let rec (|HumanBranch|_|) = function
        | Human -> Some ()
        | (Operation(_, HumanBranch _, _)) -> Some ()
        | (Operation (_, _, HumanBranch _)) -> Some ()
        | _ -> None

    let rec (|PreCalc|_|) = function
        | HumanBranch -> None
        | Number d -> Some d
        | Operation (op, PreCalc d1, PreCalc d2) -> calcOp op d1 d2 |> Some
        | Operation (_, HumanBranch, _) | Operation (_, _, HumanBranch) -> None
        | _ -> None

    let rec precalc = function
        | PreCalc d -> Number d
        | Root( p1, p2 ) -> Root (precalc p1, precalc p2)
        | t -> t

    let tree = parseOp "root" |> precalc
    let (Root(_, Number constantBranch)) = tree

    let rand = System.Random()
    let rec shoot best besth count =
        if count > 50_000 then besth else
            let r = rand.NextInt64(besth - best - 1000L, besth + best + 1000L)
            let c = calc r tree
            if c < best || c = best && r < besth then
                printfn $"h: {r} dist: {c}"
                shoot c r (count + 1)
            else shoot best besth (count + 1)

    let partTwo = shoot constantBranch constantBranch 0