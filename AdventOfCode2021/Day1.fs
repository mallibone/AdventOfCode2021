module Day1

let day1Part1 (input:string[]) =
    input
    |> Array.map (int)
    |> Array.pairwise
    |> Array.filter(fun (fst, snd) -> fst < snd)
    |> Array.length
    // |> Array.fold (fun count window -> (if window[0] < window[1] then (count+1) else count)) 0

let day1Part2 (input:string[]) =
    input
    |> Array.map (int)
    |> Array.windowed 3
    |> Array.map (fun window -> window |> Array.sum) 
    |> Array.windowed 2
    |> Array.fold (fun count window -> (if window[0] < window[1] then (count+1) else count)) 0


let executeDay testInput input= 
    day1Part1 testInput |> printfn "Day1 Part1 Test Increases: %d"
    day1Part1 input |> printfn "Day1 Part1 Increases: %d"

    day1Part2 testInput |> printfn "Day1 Part 2 Test Increases: %d"
    day1Part2 input |> printfn "Day1 Part2 Increases: %d"
    ()