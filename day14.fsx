#r "nuget: FSharpPlus"
open FSharpPlus

let state =
    [
        for str in System.IO.File.ReadAllLines "14.txt" do
            let points = str.Split " -> " |> map (sscanf "%d,%d")
            let mutable x, y = points[0]
            yield x, y
            for px, py in points[1..] do
                while (x, y) <> (px, py) do
                    x <- x + sign (px - x)
                    y <- y + sign (py - y)
                    yield x, y
    ] |> Set

let bottom = state |> map snd |> maximum

let rec moveSand floor state (x, y) =
    let empty point = state |> Set.contains point |> not
    let below = x, y + 1
    let toLeft = x - 1, y + 1
    let toRight = x + 1, y + 1
    match floor with
    | None when y > bottom -> None
    | Some fl when y = fl - 1 -> Some (x, y)
    | _ when empty below -> moveSand floor state below
    | _ when empty toLeft -> moveSand floor state toLeft
    | _ when empty toRight -> moveSand floor state toRight
    | _ -> Some (x, y) 

let rec count floor state n =
    match moveSand floor state (500, 0) with
    | None -> n
    | Some (500, 0) -> n + 1
    | Some p -> count floor (state |> Set.add p) (n + 1)

let partOne = count None state 0

let floor = Some (2 + bottom)
let partTwo = count floor state 0
