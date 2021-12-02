#time
open System.IO

let getTestInput day =
    let filename day = Path.Combine(__SOURCE_DIRECTORY__, $"Input/TestDay{day}.txt")
    File.ReadAllLines(filename day)

let getInput day =
    let filename day = Path.Combine(__SOURCE_DIRECTORY__, $"Input/Day{day}.txt")
    File.ReadAllLines(filename day)
    

let positionUpdate (hPos, vPos) (instruction:string[]) =
    match instruction[0] with
    | "forward" -> (hPos + int instruction[1], vPos)
    | "down" -> (hPos, vPos + int instruction[1])
    | "up" -> (hPos, vPos - int instruction[1])
    | _ -> (hPos, vPos)

let solveDay2 (input:string[]) =
    input
    |> Array.map (fun s -> s.Split(" "))
    |> Array.fold positionUpdate (0, 0) 
    |> fun (vPos, hPos) -> vPos * hPos

getTestInput 2
|> solveDay2
|> printfn "Test result: %d"

getInput 2
|> solveDay2
|> printfn "Result: %d"