#r "nuget:FSharpPlus"
#time "on"
open FSharpPlus

let charToDigit = function
    | '2' -> 2
    | '1' -> 1
    | '0' -> 0
    | '-' -> -1
    | '=' -> -2
    | _ -> failwith "parsing error"

let digitToChar = function
    | 2  -> '2'
    | 1  -> '1'
    | 0  -> '0'
    | -1 -> '-'
    | -2 -> '='
    | _ -> failwith "wrong digit"

let toBase10 snafu =
    let folder acc c = 5L * acc + (c |> charToDigit |> int64)
    snafu |> fold folder 0

let toSnafu number = 
    let rec loop number acc =
        let digit = number % 5L |> int
        let q, digit = 
            if digit > 2 then
                (number + 5L) / 5L, digit - 5
            else number / 5L, digit
        if q = 0L then 
            digit :: acc else
            loop q (digit :: acc)
    loop number [] |> map digitToChar |> String.ofList

let result =
    System.IO.File.ReadAllLines "25.txt"
    |> map toBase10 |> sum |> toSnafu
