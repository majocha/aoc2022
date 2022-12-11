open System.IO
open System.Text.RegularExpressions

// basically map of remainders
type Primed = Primed of Map<int, int>
module Primed =
  let private primes = [2; 3; 5; 7; 11; 13; 17; 19; 23]
  let ofInt n =
    [ for p in primes  -> p, n % p ] |> Map |> Primed
  let multiply (Primed rs1) (Primed rs2) =
    [ for p in primes -> p, (rs1[p] * rs2[p]) % p ] |> Map |> Primed
  let add (Primed rs1) (Primed rs2) =
    [ for p in primes -> p, (rs1[p] + rs2[p]) % p ] |> Map |> Primed
  let isDivisibleBy p (Primed rs) = rs[p] = 0

type Inspect = Primed -> int * Primed
type Monkey = {Id: int; Items: Primed list; Inspect: Inspect; Business: int64}

let parseMonkey text =

    // this multi line pattern will not match when this file or input is in CRLF format
    let pattern = """Monkey (?<mid>\d+):
  Starting items: (?<items>.*)
  Operation: new = old (?<operation>\*|\+|) (?<operand>\d+|old)
  Test: divisible by (?<divisibleBy>\d+)
    If true: throw to monkey (?<ifTrue>\d+)
    If false: throw to monkey (?<ifFalse>\d+)"""

    let parsed (key: string) =
        let matched = Regex.Match(text, pattern, RegexOptions.Multiline)
        matched.Groups[key].Value
      
    let parsedi = parsed >> int

    let items = [ for i in (parsed "items").Split ',' -> i |> int |> Primed.ofInt ]
    let monkeyId = parsedi "mid"
    let operation = parsed "operation"
    let primeToDivideBy = parsedi "divisibleBy"
    let ifTrue = parsedi "ifTrue"
    let ifFalse = parsedi "ifFalse"

    let inspect = fun worryLevel ->
        let p =
           match parsed "operand" with "old" -> worryLevel | _ -> parsedi "operand" |> Primed.ofInt
        let afterWorryLevel =       
            match operation with 
            | "*" -> Primed.multiply worryLevel p
            | "+" -> Primed.add worryLevel p
            | _ -> failwith "error"
        let throwTo = if  afterWorryLevel |> Primed.isDivisibleBy primeToDivideBy then ifTrue else ifFalse
        throwTo, afterWorryLevel

    { Id = monkeyId; Items = items; Inspect = inspect; Business = 0 }

let monkeys = (File.ReadAllText "11.txt").Split "\n\n" |> Seq.map parseMonkey |> Seq.toList

let turn (monkeys: Monkey list) monkeyId =
  let monkey = monkeys[monkeyId]
  let thrown = monkey.Items |> List.map monkey.Inspect |> List.groupBy fst |> Map
  let thrownTo i = thrown |> Map.tryFind i |> Option.defaultValue [] |> List.map snd
  [
    for {Id = i; Items = items} as m in monkeys do 
      if i = monkeyId then
        { m with Items = []; Business = m.Business + int64 items.Length }
      else
        { m with Items = items @ thrownTo i }
  ]

let round (monkeys: Monkey list) _ =
  [0..monkeys.Length - 1] |> List.fold turn monkeys

let partTwo = 
  [1..10000] |> List.fold round monkeys
  |> List.map (fun { Business = b } -> b) |> List.sort |> List.rev |> List.take 2 |> List.reduce (*)