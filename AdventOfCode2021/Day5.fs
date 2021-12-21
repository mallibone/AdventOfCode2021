module Day5

open System

let isHorizontalLine (coords:int[,]) = coords[0,0] = coords[1,0]
let isVerticalLine (coords:int[,]) = coords[0,1] = coords[1,1]
let isDiagonalLine (coords:int[,]) = Math.Abs(coords[0,0] - coords[1,0]) = Math.Abs(coords[0,1] - coords[1,1])

let toVector (lineCoords:int[,]) =
    match lineCoords with
    | c when isHorizontalLine c -> 
        if lineCoords[0,1] < lineCoords[1,1] then [|lineCoords[0,1] .. lineCoords[1,1]|] else [|lineCoords[0,1] .. -1 .. lineCoords[1,1]|]
        |> Array.map (fun x -> [|lineCoords[0,0];x|])
    | c when isVerticalLine c ->
        if lineCoords[0,0] < lineCoords[1,0] then [|lineCoords[0,0] .. lineCoords[1,0]|] else [|lineCoords[0,0] .. -1 .. lineCoords[1,0]|]
        |> Array.map (fun y -> [|y;lineCoords[0,1]|])
    | c when isDiagonalLine c ->
        let points = Math.Abs(lineCoords[0,0] - lineCoords[1,0])
        let diagLine =
            let xinvert = if lineCoords[0,0] - lineCoords[1,0] < 0 then 1 else -1
            let yinvert = if lineCoords[0,1] - lineCoords[1,1] < 0 then 1 else -1
            [|0 .. points|] |> Array.map (fun i -> [|lineCoords[0,0]+(i*xinvert);lineCoords[0,1]+(i*yinvert)|])
        diagLine
    | _ -> [||]
    |> array2D

let parseInput filterRules (inputLines:array<string>) =
    inputLines
    |> Array.map (fun s -> s.Split(" -> ") |> Array.map(fun ip -> ip.Split(",") |> Array.map int )|> array2D)
    |> Array.filter filterRules
    |> Array.map toVector

let createMap (lines:int[,][]) =
    let maxY = (((lines |> Array.collect (fun coords -> [|for i in [0 .. (coords |> Array2D.length1) - 1] do (coords[i,0]);(coords[i,0])|])) |> Array.max) + 1)
    let maxX = (((lines |> Array.collect (fun coords -> [|for i in [0 .. (coords |> Array2D.length1) - 1] do (coords[i,1]);(coords[i,1])|])) |> Array.max) + 1)
    Array2D.create maxY maxX 0

let drawLine (map:int[,]) (coords:int[,]) =
    for x in [0 .. (coords |> Array2D.length1) - 1] do
        map[coords[x,0],coords[x,1]] <- map[coords[x,0],coords[x,1]] + 1
    map

let drawMap (lines:int[,][]) =
    let initialMap = createMap lines
    let map = 
        lines
        |> Array.fold drawLine initialMap
    map


let executeDay testInput input =
    testInput
    |> parseInput (fun i -> isHorizontalLine i || isVerticalLine i)
    |> drawMap
    |> Seq.cast<int> 
    |> Seq.sumBy (fun p -> if p > 1 then 1 else 0)
    |> printfn "Part1 Test: %d"

    input
    |> parseInput (fun i -> isHorizontalLine i || isVerticalLine i)
    |> drawMap
    |> Seq.cast<int> 
    |> Seq.sumBy (fun p -> if p > 1 then 1 else 0)
    |> printfn "Part1: %d"

    testInput
    |> parseInput (fun i -> isHorizontalLine i || isVerticalLine i || isDiagonalLine i)
    |> drawMap
    |> Seq.cast<int> 
    |> Seq.sumBy (fun p -> if p > 1 then 1 else 0)
    |> printfn "Part1 Test: %d"

    input
    |> parseInput (fun i -> isHorizontalLine i || isVerticalLine i || isDiagonalLine i)
    |> drawMap
    |> Seq.cast<int> 
    |> Seq.sumBy (fun p -> if p > 1 then 1 else 0)
    |> printfn "Part2: %d"