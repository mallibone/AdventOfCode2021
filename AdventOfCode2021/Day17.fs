module Day17

open FSharp.Collections.ParallelSeq

type targetArea = {minX:int;maxX:int;minY:int;maxY:int}

let parseInput (input:string[]) =
    input[0].Replace("target area: x=", "").Split(", y=")
    |> Array.collect(fun s -> s.Split(".."))
    |> fun coords -> {minX = int coords[0]; maxX = int coords[1]; minY = int coords[2]; maxY = int coords[3]}

let isInTargetArea targetArea x y =
    (x >= targetArea.minX && x <= targetArea.maxX
        && y >= targetArea.minY && y <= targetArea.maxY)

let rec simulateShot (targetArea:targetArea) (trajectory:list<int*int>) x y =
    let (xpos, ypos) = trajectory.Head
    let isInOrOverTargetArea =
        (isInTargetArea targetArea xpos ypos)
        || ypos < targetArea.minY
        || xpos > targetArea.maxX
    if isInOrOverTargetArea then
        trajectory
    else
        let nextX = if x > 0 then x - 1 else x
        let nextY = y - 1
        simulateShot targetArea ((xpos + x,ypos + y)::trajectory) nextX nextY

let findVelocities targetArea =
    let simualteShot = simulateShot targetArea [(0,0)]
    [for x in [1 .. 1000] do
        for y in [targetArea.minY .. 1000] do
            yield (x,y)]
    |> PSeq.map (fun (x,y) -> ((simualteShot x y),x,y))
    |> Seq.filter (fun (t,_,_) -> t |> Seq.head |> fun (x,y) -> isInTargetArea targetArea x y)

let part1 (input:string[]) =
    input
    |> parseInput
    |> findVelocities
    |> Seq.maxBy (fun (t,_,_) -> t |> Seq.maxBy (fun (_,y) -> y))
    |> (fun (t,_,_) -> t |> Seq.map (fun (_,y) -> y))
    |> Seq.max

let part2 (input:string[]) =
    input
    |> parseInput
    |> findVelocities
    |> Seq.length

let executeDay (testInput:string[]) (input:string[]) =
    // part 1
    testInput
    |> part1
    |> printfn "Part 1 Test: %A"

    input
    |> part1
    |> printfn "Part 1: %A"

    // part 2
    testInput
    |> part2
    |> printfn "Part 2 Test: %A"

    input
    |> part2
    |> printfn "Part 2: %A"