module Day2

let positionUpdatePart2 (hPos, vPos, aim) (instruction:string[]) =
    match instruction with
    | [|"forward"; posChange|] -> (hPos + int posChange, vPos + aim * int posChange, aim)
    | [|"down"; posChange|] -> (hPos, vPos, aim + int posChange)
    | [|"up"; posChange|] -> (hPos, vPos, aim - int posChange)
    | _ -> (hPos, vPos, aim)

let positionUpdatePart1 (hPos, vPos, _) (instruction:string[]) =
    match instruction with
    | [|"forward"; posChange|] -> (hPos + int posChange, vPos, 0)
    | [|"down"; posChange|] ->  (hPos, vPos + int posChange, 0)
    | [|"up"; posChange|] -> (hPos, vPos - int posChange, 0)
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