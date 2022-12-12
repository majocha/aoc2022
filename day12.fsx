let input = System.IO.File.ReadAllText "12.txt"

module Array2D =
    let getp a (x, y) = Array2D.get a x y

let sizeX = input.IndexOf('\n')
let sizeY = input.Split('\n').Length

let pos n = n % (sizeX+1), n / (sizeX+1)
let startPos = input.IndexOf('S') |> pos
let endPos = input.IndexOf('E') |> pos

let lines = input.Split('\n')
let grid (x, y) =
    match lines[y][x] with 
    |'S' -> 'a' | 'E' -> 'z' | c -> c
    |> int

let moves (sx, sy) =
    [
        for x,y in [ -1,0; 1,0; 0,-1; 0,1 ] do
            let x, y = x + sx, y + sy
            if x >= 0 && x < sizeX && y >= 0 && y < sizeY then x, y
    ]

let walkAll start =
    let visited = Array2D.create sizeX sizeY 999999
    let notVisited (x, y) = visited[x, y] = 999999
    let updateVisited (x, y) v = if visited[x, y] > v then visited[x,y] <- v
    let validMove m p = grid m <= grid p + 1
    let validMoves p = moves p |> List.filter (validMove p) |> List.filter notVisited
    updateVisited start 0

    let rec visit dist = function
        | [] -> ()
        | toVisit ->
            [ 
                for p in toVisit do
                    updateVisited p dist
                    yield! validMoves p
            ] |> List.distinct |> visit (dist + 1)

    validMoves start |> visit 1 
    visited

let distMap = walkAll endPos
let allAs = [ for i, c in input |> Seq.indexed do if c = 'a' then i ] |> List.map pos

let partOne = Array2D.getp distMap startPos
let partTwo = allAs |> List.map (Array2D.getp distMap) |> List.min