open System.IO
open System.Text.RegularExpressions

type Inspect = int -> int * int
type Monkey = {Id: int; Items: int list; Inspect: Inspect; Business: int}
let parseMonkey text =
    printfn $"{text}"
    let number name = $"(?<{name}>\d+)"
    let toLineEnd name = $"(?<{name}>.*)"
    let operation name = $"(?<{name}>\*|\+|)"
    let operand name = $"(?<{name}>\d+|old)"
    let pattern = $"""Monkey {number "mid"}:
  Starting items: {toLineEnd "items"}
  Operation: new = old {operation "op"} {operand "p"}
  Test: divisible by {number "divisibleBy"}
    If true: throw to monkey {number "ifTrue"}
    If false: throw to monkey {number "ifFalse"}"""
    let parsed (key: string) =
        let matched = Regex.Match(text, pattern, RegexOptions.Multiline)
        matched.Groups[key].Value
    let parsedi = parsed >> int
    let items = [ for i in (parsed "items").Split ',' -> int i ]
    let monkeyId = parsedi "mid"
    let divisibleBy = parsedi "divisibleBy"
    let ifTrue = parsedi "ifTrue"
    let ifFalse = parsedi "ifFalse"

    let inspect = fun worryLevel ->
        let p =
           match parsed "p" with "old" -> worryLevel | _ -> parsedi "p"
        let afterWorryLevel =       
            match parsed "op" with 
            | "*" -> (worryLevel * p) / 3
            | "+" -> (worryLevel + p) / 3
            | _ -> failwith "error"
        let throwTo = if afterWorryLevel % divisibleBy = 0 then ifTrue else ifFalse
        throwTo, afterWorryLevel

    {Id = monkeyId; Items = items; Inspect = inspect; Business = 0}


let monkeys = 
  File.ReadAllLines "11.txt" |> Seq.chunkBySize 7 |> Seq.map (Seq.take 6 >> String.concat "\n")    //|> Array.map parseMonkey
  |> Seq.map parseMonkey |> Seq.toList

let turn (monkeys: Monkey list) monkeyId =
  let monkey = monkeys[monkeyId]
  let groups = monkey.Items |> List.map monkey.Inspect |> List.groupBy fst |> Map
  [
    for {Id = i; Items = items} as m in monkeys do
      let thrown = groups |> Map.tryFind i |> Option.defaultValue [] |> List.map snd
      if i = monkeyId then
        { m with Items = thrown; Business = m.Business + items.Length }
      else
        { m with Items = items @ thrown }
  ]

let rec round (monkeys: Monkey list) = function
  | 0 -> monkeys
  | n ->
    round ([0..monkeys.Length - 1] |> List.fold turn monkeys) (n - 1)

round monkeys 20 |> List.map (fun { Business = b } -> b) |> List.sort |> List.rev |> List.take 2 |> List.reduce (*)