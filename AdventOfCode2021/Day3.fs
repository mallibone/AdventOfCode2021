module Day3

open System

let pivot input =
    input
    |> Seq.collect(fun s -> s |> Seq.mapi(fun i e -> (i, e))) //wrap with index
    |> Seq.groupBy(fst) //group by index
    |> Seq.map(fun (i, s) -> s |> Seq.map snd) 

let solveDay3Part1 input =
    input
    |> pivot
    |> Seq.map (Seq.map (fun c -> Int32.Parse(string c)))
    |> Seq.map (fun input -> ((input |> Seq.length) / 2, input |> Seq.sum))
    |> Seq.map (fun (length, count) -> if count > length then 1 else 0)
    |> Seq.fold (fun (gamma, epsilon) num -> gamma + $"{num}", epsilon + $"{((num+1)%2)}") ("", "")
    |> (fun (gamma, epsilon) -> Convert.ToInt32(gamma, 2) * Convert.ToInt32(epsilon, 2))

let computeBinary comparison (slice:seq<int>) =
    if (comparison (slice |> Seq.length) ((slice |> Seq.sum)*2)) then 1 else 0

let inputFilter pos filter (input:string) =
    string input[pos] = filter

let rec findRating comparison input pos =
    match input with
    | [|item|] -> item
    | items -> 
        let filter = 
            items
            |> pivot
            |> Seq.map (Seq.map (fun c -> Int32.Parse(string c)))
            |> (fun x -> computeBinary comparison (x |> Seq.item pos))
        let newInput = (items |> Seq.filter (inputFilter pos (string filter))) |> Seq.toArray
        findRating comparison newInput (pos+1)

let rec solveDay3Part2 input =
    let oxygenComparison length count = count >= length
    let co2Comparison length count = count < length

    let oxygenRating = findRating oxygenComparison input 0
    let co2Rating = findRating co2Comparison input 0

    Convert.ToInt32(oxygenRating, 2) * Convert.ToInt32(co2Rating, 2)

let executeDay testInput input =
    testInput |> solveDay3Part1 |> printfn "Part1 Test result: %d"

    input |> solveDay3Part1 |> printfn "Part1 Result: %d"

    testInput |> solveDay3Part2 |> printfn "Part2 Test result: %d"

    input |> solveDay3Part2 |> printfn "Part2 Result: %d"