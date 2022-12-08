let input = System.IO.File.ReadAllLines "8.txt"
let d =input[0].Length

let trees = Array2D.init d d (fun x y -> input[y][x] |> string |> int)
let visibilityMap = Array2D.create d d 0

let look line =
    let mutable tallest = -1
    for x, y in line do
        if tallest < trees[x,y] then
            tallest <- trees[x,y]
            visibilityMap[x,y] <- 1

for i in 0..d-1 do
    look [for x in 0..d-1 -> x, i]
    look [for x = d-1 downto 0 do x, i]
    look [for y in 0..d-1 -> i, y]
    look [for y = d-1 downto 0 do i, y]

[ for x in 0..d-1 do for y in 0..d-1 do visibilityMap[x,y] ] |> List.sum

// part two

let lookFrom line =
    let x, y = line |> List.head
    let h = trees[x,y]
    let treesInDirection = line |> List.tail
    let rec countTrees total = function
        | [] -> total
        | (x,y) :: _ when trees[x,y] >= h  -> total + 1
        | _ :: rest -> countTrees (total + 1) rest
    countTrees 0 treesInDirection

let lookAllDirs x y =
    lookFrom [ for i = x downto 0 do i, y ]
    * lookFrom [ for i in x..d-1 -> i, y ]
    * lookFrom [ for i = y downto 0 do x, i ]
    * lookFrom [ for i in y..d-1 -> x, i ]

[ for x in 0..d-1 do for y in 0..d-1 do lookAllDirs x y] |> List.max
