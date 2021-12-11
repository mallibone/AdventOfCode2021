module Day11

let to2DArray (inputLines:string[]) =
    inputLines |> Array.mapi (fun y l -> l.ToCharArray() |> Array.mapi (fun x id -> (id.ToString() |> int))) |> array2D

let getNeighbourCoords (x,y) =
    let minX = 0
    let minY = 0
    let maxX = 10
    let maxY = 10

    [(-1,-1);(0,-1);(1,-1);
        (-1,0);(1,0);
        (-1,1);(0,1);(1,1);]
        |> Seq.map (fun (dx, dy) -> (dx+x,dy+y) )
        |> Seq.filter (fun (x, y) -> x >= minX && x < maxX && y >= minY && y < maxY)

let incrementEnergy (map:Map<(int * int), int>) coords =
    map
    |> Map.map(fun coord value -> 
        if (coords |> Seq.contains coord) then
            value + 1
        else
            value)

let findGlowingCoords map =
    [for x in 0 .. (map |> Array2D.length1) - 1 do
        for y in 0 .. (map |> Array2D.length2) - 1 do
            yield ((x,y),map[x,y] > 9)]
    |> List.filter snd
    |> List.map fst

let rec energiseNeighbours (map:int[,]) glowingCoords newGlowingCoords =
    match newGlowingCoords with
    | [] -> map
    | _ ->
        // remember them glowers
        let knownGlowers = (List.append glowingCoords newGlowingCoords)
        // get neighbours of new glowing squids 
        let neighboursToEnergise =
            newGlowingCoords 
            |> Seq.collect getNeighbourCoords
            // |> Seq.filter (fun c -> not (knownGlowers |> Seq.contains c))
            |> Seq.toList
        // increment neighbours by 1
        neighboursToEnergise |> Seq.iter (fun (x, y) -> map[x,y] <- map[x,y] + 1)

        // find any new glowers
        let newGlowers = 
            (findGlowingCoords map)
            |> List.filter (fun c -> not (knownGlowers |> Seq.contains c))
        
        // repeat
        energiseNeighbours map knownGlowers newGlowers

let flat2Darray array2D = 
    seq { for x in [0..(Array2D.length1 array2D) - 1] do 
                for y in [0..(Array2D.length2 array2D) - 1] do 
                yield array2D.[x, y] }

// let rec part1 remainingRounds (map:Map<(int * int), int>) =
let playRound (map:int[,]) =
    // increment all by 1
    let newMap = map |> Array2D.map ((+) 1)

    // detect glowing squids (remember them)
    let glowingCoords = findGlowingCoords newMap

    energiseNeighbours newMap [] glowingCoords
    |> Array2D.map (fun v -> if v > 9 then 0 else v)

let rec playRounds roundCound glowCount initialMap =
    match roundCound with
    | 0 -> glowCount
    | _ -> 
        let newMap = playRound initialMap
        // printfn "%A" newMap
        let newGlowCount = glowCount + (newMap |> flat2Darray |> Seq.sumBy(fun v -> if v = 0 then 1 else 0))
        playRounds (roundCound - 1) newGlowCount newMap

let rec findSuperFlash (roundCount:int) (map:int[,]) =
    match (map |> flat2Darray |> Seq.sumBy(fun v -> if v = 0 then 1 else 0)) with
    | 100 -> roundCount
    | _ -> 
        let newMap = playRound map
        // printfn "%A" newMap
        findSuperFlash (roundCount + 1) newMap

let part1 (input:string[]) =
    input
    |> to2DArray
    |> playRounds 100 0

let part2 (input:string[]) =
    input
    |> to2DArray
    |> findSuperFlash 0

let executeDay (testInput:string[]) (input:string[]) =
    // part 1
    testInput
    |> part1
    |> printfn "Part 1 Test: %d"

    input
    |> part1
    |> printfn "Part 1: %d"

    // part 2
    testInput
    |> part2
    |> printfn "Part 2 Test: %d"

    input
    |> part2
    |> printfn "Part 2: %d"