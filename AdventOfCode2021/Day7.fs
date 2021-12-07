module Day7

open System

let playPart1 crabs (startingPosition:int) =
    printfn "Starting pos: %d" startingPosition
    crabs |> Array.map (fun c -> Math.Abs(c - startingPosition)) |> Array.sum

let playPart2 crabs (meanStartingPosition:int) (avgStartingPosition:int) =
    let rec play crabs (pos:int) =
        crabs |> Array.map (fun c -> [1 .. Math.Abs(c - pos)] |> Seq.sum) |> Array.sum
    
    [meanStartingPosition .. avgStartingPosition] |> Seq.map (play crabs) |> Seq.min
    

let executeDay (testInput:string[]) (input:string[]) =
    // part 1
    testInput
    |> Array.collect (fun s -> s.Split(",") |> Array.map int) 
    |> fun crabs -> playPart1 crabs (crabs |> Array.sort |> fun a -> a[a.Length/2])
    |> printfn "Part 1 Test, spent fuel: %d"

    input
    |> Array.collect (fun s -> s.Split(",") |> Array.map int) 
    |> fun crabs -> playPart1 crabs (crabs |> Array.sort |> fun a -> a[a.Length/2])
    |> printfn "Part 1, spent fuel: %d"

    // part 2
    testInput
    |> Array.collect (fun s -> s.Split(",") |> Array.map int) 
    |> fun crabs -> playPart2 crabs (crabs |> Array.sort |> fun a -> a[a.Length/2]) (crabs |> Array.map float |> Array.average |> (fun avg -> Math.Round(avg) |> int))
    |> printfn "Part 2 Test, spent fuel: %d"

    input
    |> Array.collect (fun s -> s.Split(",") |> Array.map int) 
    |> fun crabs -> playPart2 crabs (crabs |> Array.sort |> fun a -> a[a.Length/2]) (crabs |> Array.map float |> Array.average |> (fun avg -> Math.Round(avg) |> int))
    |> printfn "Part 2, spent fuel: %d"