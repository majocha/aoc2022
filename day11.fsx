open System.IO
open System.Text.RegularExpressions

type Inspect = int64 -> int * int64
type Monkey = {Id: int; Items: int64 list; Inspect: Inspect; Business: int}
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
    let items = [ for i in (parsed "items").Split ',' -> int64 i ]
    let monkeyId = parsedi "mid"
    let divisibleBy = parsedi "divisibleBy" |> int64
    let ifTrue = parsedi "ifTrue"
    let ifFalse = parsedi "ifFalse"

    let inspect = fun (worryLevel: int64) ->
        let p =
           match parsed "p" with "old" -> worryLevel | _ -> parsedi "p"
           |> int64
        let afterWorryLevel =       
            match parsed "op" with 
            | "*" -> (worryLevel * p) / 3L
            | "+" -> (worryLevel + p) / 3L
            | _ -> failwith "error"
        let throwTo = if afterWorryLevel % divisibleBy = 0L then ifTrue else ifFalse
        throwTo, afterWorryLevel

    {Id = monkeyId; Items = items; Inspect = inspect; Business = 0}

let monkeys = 
  File.ReadAllLines "11.txt" |> Seq.chunkBySize 7 |> Seq.map (Seq.take 6 >> String.concat "\n")    //|> Array.map parseMonkey
  |> Seq.map parseMonkey |> Seq.toList

let turn (monkeys: Monkey list) monkeyId =
  let monkey = monkeys[monkeyId]
  let thrown = monkey.Items |> List.map monkey.Inspect |> List.groupBy fst |> Map
  let thrownTo i = thrown |> Map.tryFind i |> Option.defaultValue [] |> List.map snd
  [
    for {Id = i; Items = items} as m in monkeys do 
      if i = monkeyId then
        { m with Items = []; Business = m.Business + items.Length }
      else
        { m with Items = items @ thrownTo i }
  ]

let round (monkeys: Monkey list) _ =
  [0..monkeys.Length - 1] |> List.fold turn monkeys

[1..20] |> List.fold round monkeys
|> List.map (fun { Business = b } -> b) |> List.sort |> List.rev |> List.take 2 |> List.reduce (*)