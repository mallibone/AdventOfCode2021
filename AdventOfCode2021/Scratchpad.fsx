#time
open System.IO
open System

let getTestInput day =
    let filename day = Path.Combine(__SOURCE_DIRECTORY__, $"Input/TestDay{day}.txt")
    File.ReadAllLines(filename day)

let getInput day =
    let filename day = Path.Combine(__SOURCE_DIRECTORY__, $"Input/Day{day}.txt")
    File.ReadAllLines(filename day)

let parseInput (inputLines:array<string>) =
    inputLines
    // parse input lines
    |> Array.map (fun s -> s.Split(" -> ") |> Array.map(fun ip -> ip.Split(",") |> Array.map int )|> array2D)
    // filter only horizontal and vertical lines
    |> Array.filter(fun coords -> coords[0,0] = coords[1,0] || coords[0,1] = coords[1,1])

let createMap (lines:int[,][]) =
    let maxY = ((lines |> Array.collect (fun coords -> [|(coords[0,0]);(coords[1,0])|])) |> Array.max) + 1
    let maxX = ((lines |> Array.collect (fun coords -> [|(coords[0,1]);(coords[1,1])|])) |> Array.max) + 1
    Array2D.create maxX maxY 0

let drawLine (map:int[,]) (line:int[,]) =
    let ycoords = if line[0,0] < line[1,0] then [line[0,0] .. line[1,0]] else [line[0,0] .. -1 .. line[1,0]]
    let xcoords = if line[0,1] < line[1,1] then [line[0,1] .. line[1,1]] else [line[0,1] .. -1 .. line[1,1]]
    // printfn $"line: %A{line}"
    // printfn $"xcoords: %A{xcoords} ycoords: %A{ycoords}"
    for y in ycoords do
        for x in xcoords do
            map[x,y] <- map[x,y] + 1
    map

let drawMap (lines:int[,][]) =
    let initialMap = createMap lines
    lines
    |> Array.fold drawLine initialMap

getTestInput 5
|> parseInput
|> drawMap
|> Seq.cast<int> 
|> Seq.sumBy (fun p -> if p > 1 then 1 else 0)

getInput 5
|> parseInput
|> drawMap
|> Seq.cast<int> 
|> Seq.sumBy (fun p -> if p > 1 then 1 else 0)