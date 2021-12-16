#time
open System.IO
open System

let getTestInput day =
    let filename day = Path.Combine(__SOURCE_DIRECTORY__, $"Input/TestDay{day}.txt")
    File.ReadAllLines(filename day)

let getInput day =
    let filename day = Path.Combine(__SOURCE_DIRECTORY__, $"Input/Day{day}.txt")
    File.ReadAllLines(filename day)

let getMap (input:string[]) =
    input
    |> Array.map (fun s -> s.ToCharArray() |> Array.map (fun c -> Convert.ToInt32(c.ToString())))
    |> array2D

let getDirectNeighbourCoords maxX maxY (x,y) =
    let minX = 0
    let minY = 0

    [(0,-1);
        (-1,0);(1,0);
        (0,1);]
        |> Seq.map (fun (dx, dy) -> (dx+x,dy+y))
        |> Seq.filter (fun (x, y) -> x >= minX && x < maxX && y >= minY && y < maxY)

let findPaths (map:int[,]) (pathPoints:list<int*int>) =
    // printfn "%A" pathPoints
    let (lpX, lpY) = pathPoints.Head
    if lpY = (map |> Array2D.length1) - 1 then 
        [pathPoints]
    else
        let getNeighbourCoords = getDirectNeighbourCoords (map |> Array2D.length2) (map |> Array2D.length1)

        let neighbours =
            getNeighbourCoords (lpX, lpY)
            |> Seq.filter(fun coord -> not (pathPoints |> List.contains coord)) 
            |> Seq.toList
            
        // if neighbours.Length = 0 then
        //     []
        // else
        let minDangerValue = 
            if neighbours.Length > 0 then
                neighbours 
                |> Seq.map (fun (x,y) -> map[y,x]) 
                |> Seq.min
            else
                0
        // printfn "min %A x %d y %d" minDangerValue lpX lpY

        neighbours 
        |> Seq.filter(fun (x,y) -> map[y,x] = minDangerValue)
        |> Seq.map(fun coord -> coord::pathPoints)
        |> Seq.toList

let part1 (input:string[]) =
    let map = getMap input
    let getNeighbourCoords = getDirectNeighbourCoords (map |> Array2D.length2) (map |> Array2D.length1)
    let maxY = (map |> Array2D.length1) - 1
    let rec findRoute (map:int[,]) (paths:list<list<int*int>>) =
        // printfn "%A" paths
        if paths |> Seq.map Seq.head |> Seq.forall (fun (_,y) -> y = maxY) then
            paths
        else
            let newPaths =
                paths 
                |> List.collect (findPaths map)
                |> List.filter (fun c -> not c.IsEmpty)
                |> List.distinct
            
            findRoute map newPaths

    findRoute map [[(0,0)]]
    // |> Seq.map(fun coords -> coords |> Seq.rev |> Seq.skip 1 |> Seq.map(fun (x,y) -> (x,y,map[y,x])) |> Seq.toList)
    // |> Seq.toList


// getTestInput 15
getInput 15
|> part1
