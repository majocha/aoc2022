#r "nuget:FSharpPlus"
#time "on"
open FSharpPlus

let input = System.IO.File.ReadAllLines "21.txt"

type Op = Add | Sub | Div | Mult
and Yell = Number of int64 | Operation of Op * string * string

let (|OPERATION|_|) = trySscanf "%s %s %s"
let (|NUMBER|_|) = trySscanf "%d"
let (|OP|_|) = function
    "+" -> Some Add | "-" -> Some Sub | "/" -> Some Div | "*" -> Some Mult | _ -> None
let parseLine s =
    let name, yelled = sscanf "%s: %s" s
    match yelled with
    | NUMBER x -> name,  Number x
    | OPERATION (m1, OP op, m2) -> name, Operation (op, m1, m2)
    | _ -> failwith "error"

let monkeys = input |> Seq.map parseLine |> Map

let rec calc ms n =
    match ms |> Map.find n with
    | Number x -> x
    | Operation (op, n1, n2) ->
        let calc = calc ms
        match op with
        | Add -> calc n1 + calc n2
        | Sub -> calc n1 - calc n2
        | Div -> calc n1 / calc n2
        | Mult -> calc n1 * calc n2

calc monkeys "root"

let invert monkeys =
    [
        for name, yell in monkeys |> Map.toSeq do
            match yell with
            | Number d -> name, Number d
            | Operation (op, n1, n2) as oper ->
                match op with
                | Add ->
                    n1, Operation (Sub, name, n2)
                    n2, Operation (Sub, name, n1)
                | Sub ->
                    n1, Operation (Add, name, n2)
                    n2, Operation (Add, name, n1)
                | Mult ->
                    n1, Operation (Div, name, n2)
                    n2, Operation (Div, name, n1)
                | Div ->
                    n1, Operation (Mult, name, n2)
                    n2, Operation (Mult, name, n1)

    ] |> Map


