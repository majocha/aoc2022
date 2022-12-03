let priority =
    let letters = ['a'..'z'] @ ['A'..'Z'] |> List.toArray
    fun (c: char) -> System.Array.IndexOf(letters, c) + 1

let rucksack (s: string) =
    let middle = s.Length / 2
    let first = Set s[..middle-1]
    let second = Set s[middle..]
    Set.intersect first second |> Seq.head |> priority

let input = System.IO.File.ReadAllLines "3.txt"

input |> Seq.map rucksack |> Seq.sum

//part two
let findBadge rs =
    rs |> Seq.map Set |> Set.intersectMany |> Seq.head |> priority

input |> Seq.chunkBySize 3 |> Seq.map findBadge |> Seq.sum