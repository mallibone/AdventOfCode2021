module Day9

let flat2Darray array2D = 
    seq { for x in [0..(Array2D.length1 array2D) - 1] do 
                for y in [0..(Array2D.length2 array2D) - 1] do 
                    yield array2D.[x, y] }

let parseInput (input:string[]) = 
    input
    |> Array.map (fun s -> s.ToCharArray() |> Array.map (fun c -> c.ToString() |> int))
    |> array2D


let getAdjacentNeighbours (twoDarray:'a[,]) x y =
    let minX = x-1
    let maxX = x + 1
    let minY = y-1
    let maxY = y + 1

    [ if minY >= 0 then (twoDarray[x,minY], x, minY);
      if minX >= 0 then (twoDarray[minX,y], minX, y);  
      if maxX < (twoDarray |> Array2D.length1) then (twoDarray[maxX,y], maxX, y); 
      if maxY < (twoDarray |> Array2D.length2) then (twoDarray[x,maxY], x, maxY); ]
        

let filterLowPoints (floorSections:int[,]) =
    floorSections 
    |> Array2D.mapi (fun x y v -> ((getAdjacentNeighbours floorSections x y) |> Seq.forall(fun (nv,_,_) -> v < nv), v, x, y)) 

let rec findPool (floorSections:int[,]) (pool:(int*int*int)list) (lowPoints:(int*int)list) =
    match lowPoints with
    | [] -> pool
    | [poolPoint] -> 
        let x = fst poolPoint
        let y = snd poolPoint
        let newPoolPoints = 
            getAdjacentNeighbours floorSections x y
            |> List.filter(fun (n,x,y) -> n <> 9 && not (pool |> Seq.contains(n,x,y)))
            |> Seq.map(fun (_,x,y) -> (x,y))
            |> Seq.toList
        let newPool = ((floorSections[x,y],x,y)::pool)
        findPool floorSections newPool newPoolPoints
    | poolPoint::morePoolPoints -> 
        let x = fst poolPoint
        let y = snd poolPoint
        let newPoolPoints = 
            getAdjacentNeighbours floorSections x y
            // |> List.filter(fun (n,x,y) -> n <> 9 && not (pool |> Seq.contains(n,x,y)))
            |> List.filter(fun (n,x,y) -> n <> 9 && not (pool |> Seq.contains(n,x,y)) && not (morePoolPoints |> Seq.contains(x,y)))
            |> Seq.map(fun (_,x,y) -> (x,y))
            |> Seq.toList
        let newPool = ((floorSections[x,y],x,y)::pool)
        findPool floorSections newPool (morePoolPoints@newPoolPoints)

let findPools (floorSections:int[,]) =
    let lowPoints = 
        floorSections
        |> filterLowPoints
        |> flat2Darray
        |> Seq.filter (fun (isLowPoint,_,_,_) -> isLowPoint)
        |> Seq.map(fun (_,_,x,y) -> (x,y))
        |> Seq.toList

    lowPoints 
    |> Seq.map (fun lp -> findPool floorSections [] [lp])
    |> Seq.map (fun pool -> pool |> Seq.sortDescending)
    |> Seq.distinct
    |> Seq.map (fun p -> p |> Seq.length)

let part1 input =
    input
    |> parseInput
    |> filterLowPoints
    |> flat2Darray
    |> Seq.filter (fun (isLowPoint,_,_,_) -> isLowPoint)
    |> Seq.map (fun (_,value,_,_) -> value)
    |> Seq.map (fun i -> i + 1)
    |> Seq.sum

let part2 input = 
    input
    |> parseInput
    |> findPools
    |> Seq.sortDescending
    |> Seq.take 3
    |> Seq.fold(fun state n -> n * state) 1

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