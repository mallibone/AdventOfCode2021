module Day6

open System

let simulate iterations (fishes:int64[]) =
    let rec simulateRounds iteration (fishAgeGroups:int64[]) =
        if iteration > 0 then
            // [|8 .. -1 .. 0|] |> Array.map(fun i -> fishAgeGroups[] |> Seq.filter(fun fish -> fish = i) |> Seq.length)
            let nextFishAgeGroup =
                [|0 .. 8|] |> Array.map(fun i -> 
                    match i with
                    | 8 -> fishAgeGroups[0]
                    | 6 -> fishAgeGroups[i+1] + fishAgeGroups[0]
                    | _ -> fishAgeGroups[i+1])
            simulateRounds (iteration - 1) nextFishAgeGroup
        else
            fishAgeGroups

    let ageGroups = [|0 .. 8|] |> Array.map(fun i -> fishes |> Seq.filter(fun fish -> fish = i) |> Seq.length |> int64)

    simulateRounds iterations ageGroups

let executeDay (testInput:string[]) (input:string[]) =
    let daysPart1 = 80
    testInput
    |> Array.collect (fun s -> s.Split(",") |> Array.map int64)
    |> simulate daysPart1
    |> Seq.sum
    |> printfn "Part 1 Test, Fishes after %d days: %A" daysPart1

    input
    |> Array.collect (fun s -> s.Split(",") |> Array.map int64)
    |> simulate daysPart1
    |> Seq.sum
    |> printfn "Part 1, Fishes after %d days: %A" daysPart1

    let daysPart2 = 256
    testInput
    |> Array.collect (fun s -> s.Split(",") |> Array.map int64)
    |> simulate daysPart2
    |> Seq.sum
    |> printfn "Part 2 Test, Fishes after %d days: %A" daysPart2

    input
    |> Array.collect (fun s -> s.Split(",") |> Array.map int64)
    |> simulate daysPart2
    |> Seq.sum
    |> printfn "Part 2 Test, Fishes after %d days: %A" daysPart2
// |> Seq.length