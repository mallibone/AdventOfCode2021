module Day2

let positionUpdatePart2 (hPos, vPos, aim) (instruction:string[]) =
    match instruction[0] with
    | "forward" -> (hPos + int instruction[1], vPos + aim * int instruction[1], aim)
    | "down" -> (hPos, vPos, aim + int instruction[1])
    | "up" -> (hPos, vPos, aim - int instruction[1])
    | _ -> (hPos, vPos, aim)

let positionUpdatePart1 (hPos, vPos, _) (instruction:string[]) =
    match instruction[0] with
    | "forward" -> (hPos + int instruction[1], vPos, 0)
    | "down" -> (hPos, vPos + int instruction[1], 0)
    | "up" -> (hPos, vPos - int instruction[1], 0)
    | _ -> (hPos, vPos, 0)

let solveDay2 positionUpdate (input:string[]) =
    input
    |> Array.map (fun s -> s.Split(" "))
    |> Array.fold positionUpdate (0, 0, 0) 
    |> fun (vPos, hPos, _) -> vPos * hPos


let executeDay testInput input =
    testInput
    |> solveDay2 positionUpdatePart1
    |> printfn "Part1 Test result: %d"

    input
    |> solveDay2 positionUpdatePart1
    |> printfn "Part1 Result: %d"

    testInput
    |> solveDay2 positionUpdatePart2
    |> printfn "Part2 Test result: %d"

    input
    |> solveDay2 positionUpdatePart2
    |> printfn "Part2 Result: %d"