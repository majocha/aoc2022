let input = System.IO.File.ReadAllLines "8.txt"
let d = input.Length

let trees = Array2D.init d d (fun x y -> input[y][x] |> string |> int)

let look line =
    let folder (visible, tallest) = function
        | x, y when trees[x, y] > tallest -> (x, y) :: visible, trees[x, y]
        | _ -> (visible, tallest)
    line |> List.fold folder ([], -1) |> fst

let dirs i =
    let row = [ for x in 0..d-1 -> x, i ]
    let col = [ for y in 0..d-1 -> i, y ]
    [ row; col; row |> List.rev; col |> List.rev]

let partOne =
    [0 .. d - 1] |> List.collect (dirs >> List.collect look) |> List.distinct |> List.length

let dirs2 x y = [
    [ for i in x..d-1 -> i, y ]
    [ for i = x downto 0 do i, y ]
    [ for i = y downto 0 do x, i ]
    [ for i in y..d-1 -> x, i ]
]

let lookFrom h line =
    let treesInDirection = line |> List.tail
    let rec countTrees total = function
        | [] -> total
        | (x,y) :: _ when trees[x,y] >= h  -> total + 1
        | _ :: rest -> countTrees (total + 1) rest
    countTrees 0 treesInDirection

let value x y h = dirs2 x y |> List.map (lookFrom h) |> List.reduce (*)

let partTwo =
    trees |> Array2D.mapi value |> Seq.cast<int> |> Seq.max
