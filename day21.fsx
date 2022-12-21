#r "nuget:FSharpPlus"
#time "on"
open FSharpPlus

let input = System.IO.File.ReadAllLines "21.txt"

type Op = Add | Sub | Div | Mult
and Yell = Number of int64 | Operation of string * Op * string

let (|POperation|_|) = trySscanf "%s %s %s"
let (|PNumber|_|) = trySscanf "%d"
let (|POperator|_|) = function
    "+" -> Some Add | "-" -> Some Sub | "/" -> Some Div | "*" -> Some Mult | _ -> None
let parseLine s =
    let name, yelled = sscanf "%s: %s" s
    match yelled with
    | PNumber x -> name,  Number x
    | POperation (m1, POperator op, m2) -> name, Operation (m1, op, m2)
    | _ -> failwith "error"

let monkeys = input |> Seq.map parseLine |> Map

let rec calc = function
    | Number x -> x
    | Operation (m1, op, m2) ->
        let m1, m2 = monkeys[m1], monkeys[m2]
        match op with
        | Add -> calc m1 + calc m2
        | Sub -> calc m1 - calc m2
        | Div -> calc m1 / calc m2
        | Mult -> calc m1 * calc m2

calc monkeys["root"]

let invert monkeys =
    [
        for name, yell in monkeys |> Map.toSeq do
            match yell with
            | Number d -> name, Number d
            | Operation (n1, op, n2) as oper ->
                match op with
                | Add ->
                    n1, Operation (name, Sub, n2)
                    n2, Operation (name, Sub, n1)
                | Sub ->
                    n1, Operation (name, Add, n2)
                    n2, Operation (name, Add, n1)
                | Mult ->
                    n1, Operation (name, Div, n2)
                    n2, Operation (name, Div, n1)
                | Div ->
                    n1, Operation (name, Mult, n2)
                    n2, Operation (name, Mult, n1)

    ] |> Map

let inverted = invert monkeys

let rec calci = function
    | Number x -> x
    | Operation ("root", _, m) | Operation (m, _, "root") ->
        calc monkeys[m]
    | Operation (m1, op, m2) ->
        let m1, m2 = inverted[m1], inverted[m2]
        match op with
        | Add -> calci m1 + calci m2
        | Sub -> calci m1 - calci m2
        | Div -> calci m1 / calci m2
        | Mult -> calci m1 * calci m2

calci inverted["humn"]


