#time
open System.IO
open System

let getTestInput day =
    let filename day = Path.Combine(__SOURCE_DIRECTORY__, $"Input/TestDay{day}.txt")
    File.ReadAllLines(filename day)

let getInput day =
    let filename day = Path.Combine(__SOURCE_DIRECTORY__, $"Input/Day{day}.txt")
    File.ReadAllLines(filename day)

type State = | On | Off
type CoordinateRange = {Min:int;Max:int}
type CoordinateStateChange = {State:State;X:CoordinateRange;Y:CoordinateRange;Z:CoordinateRange}

let parseInput (input:string[]) =
    let splitToTuple (splitter:string) (value:string) = value.Split(splitter) |> fun s -> s[0],s[1]
    let takeSecond (splitter:string) (value:string) = (value.Split(splitter))[1]
    let parseLine (line:string) =
        let stateString, coordinates = splitToTuple " " line
        let coordinateRanges = coordinates.Split(",") 
                                |> Array.map (fun s -> ((s |> takeSecond "=") |> splitToTuple ".."))
                                |> Array.map (fun (min, max) -> (int min) + 50, (int max) + 50)
        // let coordinateRanges = coordinates.Split(",") |> Array.map (fun s -> ((s |> takeSecond "="))) 
        // printfn "%A" coordinateRanges
        // let coordinateRanges = coordinateRanges |> Array.map (fun s -> s |> splitToTuple "..")
        // printfn "%A" coordinateRanges
        let state = match stateString with | "on" -> On | _ -> Off
        {
            State = state; 
            X = {Min = (fst coordinateRanges[0]); Max = (snd coordinateRanges[0])}; 
            Y = {Min = (fst coordinateRanges[1]); Max = (snd coordinateRanges[1])}; 
            Z = {Min = (fst coordinateRanges[2]); Max = (snd coordinateRanges[2])}
        }

    input 
    |> Array.map parseLine

let applyReboot (state:State[,,]) (rebootInstruction:CoordinateStateChange) =
    let isOutOfRange x y z = x.Max < 0 || x.Min > 100 || y.Max < 0 || y.Min > 100 || z.Max < 0 || z.Min > 100
    if isOutOfRange rebootInstruction.X rebootInstruction.Y rebootInstruction.Z then
        state
    else
        let minX = Math.Max(0, rebootInstruction.X.Min)
        let maxX = Math.Min(100, rebootInstruction.X.Max)
        let minY = Math.Max(0, rebootInstruction.Y.Min)
        let maxY = Math.Min(100, rebootInstruction.Y.Max)
        let minZ = Math.Max(0, rebootInstruction.Z.Min)
        let maxZ = Math.Min(100, rebootInstruction.Z.Max)
        for x in minX..maxX do
            for y in minY..maxY do
                for z in minZ..maxZ do
                    state[x,y,z] <- rebootInstruction.State
        state

let applyFullReboot (state:State[,,]) (rebootInstruction:CoordinateStateChange) =
    let isOutOfRange x y z = x.Max < 0 || x.Min > 100 || y.Max < 0 || y.Min > 100 || z.Max < 0 || z.Min > 100
    if isOutOfRange rebootInstruction.X rebootInstruction.Y rebootInstruction.Z then
        state
    else
        let minX = Math.Max(0, rebootInstruction.X.Min)
        let maxX = Math.Min(100, rebootInstruction.X.Max)
        let minY = Math.Max(0, rebootInstruction.Y.Min)
        let maxY = Math.Min(100, rebootInstruction.Y.Max)
        let minZ = Math.Max(0, rebootInstruction.Z.Min)
        let maxZ = Math.Min(100, rebootInstruction.Z.Max)
        for x in minX..maxX do
            for y in minY..maxY do
                for z in minZ..maxZ do
                    state[x,y,z] <- rebootInstruction.State
        state

let flat3DArray (array3D:'a[,,]) =
    let maxX = (array3D |> Array3D.length1) - 1
    let maxY = (array3D |> Array3D.length2) - 1
    let maxZ = (array3D |> Array3D.length3) - 1
    seq {
        for x in 0..maxX do
        for y in 0..maxY do
            for z in 0..maxZ do
                yield array3D[x,y,z] }

let part1 (input:string[]) =
    let initialResetState = Array3D.init 101 101 101 (fun _ _ _ -> Off)
    parseInput input 
    |> Seq.fold applyReboot initialResetState
    |> flat3DArray
    |> Seq.filter (fun s -> s = On)
    |> Seq.length

let part2 (input:string[]) =
    let initialResetState = Array3D.init 101 101 101 (fun _ _ _ -> Off)
    parseInput input 
    |> Seq.fold applyReboot initialResetState
    |> flat3DArray
    |> Seq.filter (fun s -> s = On)
    |> Seq.length

getTestInput 22
getInput 22
|> part1

// [-50..50] |> Seq.length