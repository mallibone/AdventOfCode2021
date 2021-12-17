#time
open System.IO
open System

let getTestInput day =
    let filename day = Path.Combine(__SOURCE_DIRECTORY__, $"Input/TestDay{day}.txt")
    File.ReadAllLines(filename day)

let getInput day =
    let filename day = Path.Combine(__SOURCE_DIRECTORY__, $"Input/Day{day}.txt")
    File.ReadAllLines(filename day)

type targetArea = {minX:int;maxX:int;minY:int;maxY:int}

let parseInput (input:string[]) =
    input[0].Replace("target area: x=", "").Split(", y=")
    |> Array.collect(fun s -> s.Split(".."))
    |> fun coords -> {minX = int coords[0]; maxX = int coords[1]; minY = int coords[2]; maxY = int coords[3]}

let isInTargetArea targetArea x y =
    // printfn $"Targetarea x: {targetArea.minX} - {targetArea.maxX} / Targetaera y: {targetArea.minY} - {targetArea.maxY}"
    // printfn $"x: {x} / y: {y}"
    (x >= targetArea.minX && x <= targetArea.maxX
        && y >= targetArea.minY && y <= targetArea.maxY)

let rec simulateShot (targetArea:targetArea) (trajectory:list<int*int>) x y =
    let (xpos, ypos) = trajectory.Head
    let isInOrOverTargetArea =
        (isInTargetArea targetArea xpos ypos)
        || ypos < targetArea.minY
        || xpos > targetArea.maxX
    if isInOrOverTargetArea then
        // printfn $"x: {xpos} / y: {ypos}"
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
    |> Seq.map (fun (x,y) -> ((simualteShot x y),x,y))
    |> Seq.filter (fun (t,_,_) -> t |> Seq.head |> fun (x,y) -> isInTargetArea targetArea x y)

let part1 (targetArea:targetArea) =
    findVelocities targetArea
    |> Seq.maxBy (fun (t,_,_) -> t |> Seq.maxBy (fun (_,y) -> y))
    |> (fun (t,_,_) -> t |> Seq.map (fun (_,y) -> y))
    |> Seq.max

let part2 (targetArea:targetArea) =
    findVelocities targetArea
    |> Seq.length

getInput 17
// getTestInput 17
|> parseInput
|> part2