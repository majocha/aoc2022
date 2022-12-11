open System.IO
open System.Text.RegularExpressions

type Monkey = {Id: int; Items: int list; Throw: int -> int * int}
let parseMonkey text =
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
    //let items = [ for i in (parsed "items").Split ',' -> int i ]
    let monkeyId = parsedi "mid"
    let divisibleBy = parsedi "divisibleBy"
    let ifTrue = parsedi "ifTrue"
    let ifFalse = parsedi "ifFalse"
    let monkeyThrowItem = fun worryLevel ->
        let p =
           match parsed "p" with "old" -> worryLevel | _ -> parsedi "p"
        let worryLevel =       
            match parsed "op" with 
            | "*" -> worryLevel * p 
            | "+" -> worryLevel + p
            | _ -> failwith "error"
        let throwTo = if worryLevel % divisibleBy = 0 then ifTrue else ifFalse
        throwTo, worryLevel / 3
    //{ Id = monkeyId; Items = items; Throw = monkeyThrowItem }
    parsed "items"



parseMonkey """Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0"""

let m = 
    (File.ReadAllText "11.txt").Split("\r\n\r\n")
    //|> Array.map parseMonkey

printf $"'{m[0]}'"
parseMonkey (m[0] |> string)
parseMonkey """Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3"""  
