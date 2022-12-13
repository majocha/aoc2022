type Tree = Leaf of int | Node of Tree list

let token pattern input =
    let m = System.Text.RegularExpressions.Regex.Match(input, $"^{pattern}")
    if m.Success then Some (m.Value, input[m.Length..]) else None

let (|Int|_|) = token @"\d+" >> Option.map (fun (s, rest) -> int s, rest)
let (|LeftP|_|) = token @"\[" >> Option.map snd
let (|RightP|_|) = token @"\]" >> Option.map snd
let (|Coma|_|) = token @"," >> Option.map snd 

let rec (|Packet|_|) = function
    | LeftP(PList [] (ps, RightP rest)) -> Some (Node (ps |> List.rev), rest)
    | Int (d, rest) -> Some (Leaf d, rest)
    | _ -> None

and (|PList|_|) acc  = function
    | Packet(p, Coma rest) -> (|PList|_|) (p :: acc) rest
    | Packet (p, rest) -> Some ((p::acc), rest)
    | rest -> Some (acc, rest)

let parse = function
    | Packet (p, "") -> p
    | input -> failwith $"parse error on input: {input}"

let rec compareTrees left right =
    match left, right with
    | Leaf l, Leaf r -> sign (l - r)
    | Leaf d, right -> compareTrees (Node [Leaf d]) right
    | left, Leaf d -> compareTrees left (Node [Leaf d])
    | Node ls, Node rs ->
        match ls, rs with
        | l :: ls, r :: rs ->
            let q = compareTrees l r
            if q = 0 then compareTrees (Node ls) (Node rs) else q
        | [], [] -> 0
        | _, [] -> 1
        | [], _ -> -1

let p2, p6 = parse "[[2]]", parse "[[6]]"
let packets = 
    System.IO.File.ReadAllLines "13.txt"
    |> Seq.filter (System.String.IsNullOrEmpty >> not)
    |> Seq.map parse |> Seq.append [p2; p6] |> Seq.toArray
    
Array.sortInPlaceWith compareTrees packets
let findIndex p = System.Array.IndexOf(packets, p) + 1
let partTwo = findIndex p2 * findIndex p6
