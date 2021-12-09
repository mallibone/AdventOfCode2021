#time
open System.IO
open System

let getTestInput day =
    let filename day = Path.Combine(__SOURCE_DIRECTORY__, $"Input/TestDay{day}.txt")
    File.ReadAllLines(filename day)

let getInput day =
    let filename day = Path.Combine(__SOURCE_DIRECTORY__, $"Input/Day{day}.txt")
    File.ReadAllLines(filename day)

let flat2Darray array2D = 
    seq { for x in [0..(Array2D.length1 array2D) - 1] do 
                for y in [0..(Array2D.length2 array2D) - 1] do 
                    yield array2D.[x, y] }

let parseInput (input:string[]) = 
    input
    |> Array.map (fun s -> s.ToCharArray() |> Array.map (fun c -> c.ToString() |> int))
    |> array2D


let getNeighbours map (x, y) =
    [ (-1, 0); (1, 0); (0, -1); (0, 1) ]
    |> Seq.map (fun (dx, dy) -> (x + dx, y + dy))
    |> Seq.choose
        (fun l ->
            map
            |> Map.tryFind l
            |> Option.map (fun v -> l, v))

let getAdjacentNeighbours (twoDarray:'a[,]) x y =
    let minX = x-1
    let maxX = x + 1
    // let maxX = if x < ((twoDarray |> Array2D.length1) - 1) then x + 1 else x
    let minY = y-1
    let maxY = y + 1
    // let maxY = if y < ((twoDarray |> Array2D.length2) - 1) then y + 1 else y

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


    // let x = fst lowPoint
    // let y = snd lowPoint
    // let newPoolFields = 
    //     (getAdjacentNeighbours floorSections x y) 
    //     |> List.filter(fun (n,x,y) -> n <> 9 && not (pool |> Seq.contains(n,x,y)))
    // // printfn $"gna {x} {y}"

    // match newPoolFields with
    // | [] -> pool
    // | _ -> 
    //     let newPool = List.append pool newPoolFields
    //     newPoolFields |> List.collect(fun (_,x,y) -> findPool floorSections newPool (x,y))

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


// Part 1
// getTestInput 9
// getInput 9
// |> parseInput
// |> filterLowPoints
// |> flat2Darray
// |> Seq.filter (fun (isLowPoint,_,_,_) -> isLowPoint)
// |> Seq.map (fun (_,value,_,_) -> value)
// |> Seq.map (fun i -> i + 1)
// |> Seq.sum

getInput 9
// getTestInput 9
|> parseInput
|> findPools
|> Seq.sortDescending
|> Seq.take 3
|> Seq.fold(fun state n -> n * state) 1
|> printfn "%A"