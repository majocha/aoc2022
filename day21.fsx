#r "nuget:FSharpPlus"
#time "on"
open FSharpPlus

let input = System.IO.File.ReadAllLines "21.txt"

type Op = Add | Sub | Div | Mult
and Yell = Number of string * int64 | Operation of string * Op * Yell * Yell

let (|OPERATION|_|) = trySscanf "%s %s %s"
let (|NUMBER|_|) = trySscanf "%d"
let (|OP|_|) = function
    "+" -> Some Add | "-" -> Some Sub | "/" -> Some Div | "*" -> Some Mult | _ -> None
let parseLine s = sscanf "%s: %s" s

let lines = input |> Seq.map parseLine |> Map

let rec parseOp name =
    match lines[name] with
    | NUMBER x -> Number (name, x)
    | OPERATION (n1, OP op, n2) -> Operation (name, op, parseOp n1, parseOp n2)
    | _ -> failwith "error"

let tree = parseOp "root"

let calcOp op a b =
    match op with Add -> a + b | Sub -> a - b | Mult -> a * b | Div -> a / b

let rec calc = function Number (_, d) -> d | Operation (_, op, p1, p2) -> calcOp op (calc p1) (calc p2)

let partOne = calc tree
