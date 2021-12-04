#time
open System.IO
open System

let getTestInput day =
    let filename day = Path.Combine(__SOURCE_DIRECTORY__, $"Input/TestDay{day}.txt")
    File.ReadAllLines(filename day)

let getInput day =
    let filename day = Path.Combine(__SOURCE_DIRECTORY__, $"Input/Day{day}.txt")
    File.ReadAllLines(filename day)

let pivot input =
    input
    |> Seq.collect Seq.indexed // index the input
    |> Seq.groupBy fst //group by index
    |> Seq.map(snd >> Seq.map snd)

let rec parseBingofields (input:seq<string>) (bingoFields:seq<seq<seq<string*int>>>) =
    let getSkip collection = (Math.Min(6, (collection |> Seq.length)))
    match Seq.isEmpty input with
    | true -> bingoFields
    | false ->
        let newBingoField = input |> Seq.take 5 |> Seq.map (fun s -> s.Split(" ") |> Seq.filter (fun s -> not (String.IsNullOrWhiteSpace(s))) |> Seq.map(fun s -> (s,0)))
        let bingoFields = if Seq.isEmpty bingoFields  then seq { newBingoField } else (Seq.append bingoFields (seq {newBingoField}))
        printfn "input left: %A" input
        parseBingofields (input |> Seq.skip (getSkip input)) bingoFields

let isBingo fields =
    fields
    |> Seq.filter (fun bingoBoardFields -> bingoBoardFields |> Seq.exists (Seq.forall (fun x -> (snd x) = 1)))

let sumUnmarkedBingoFields fields =
    fields |> Seq.sumBy (fun bingoFields -> bingoFields |> Seq.collect(fun bingoRowFields -> bingoRowFields |> Seq.filter(fun field -> (snd field) = 0)) |> Seq.sumBy(fun field -> int (fst field)))

let rec playBingo (numbers:list<string>) bingoFields =
    let play head tail =
        // increment numbers that match
        let newBingoFields =
            bingoFields
            |> Seq.map (fun field -> field |> Seq.map (fun lineItems -> lineItems |> Seq.map(fun e -> if fst e = head then (fst e, (snd e) + 1) else (fst e, snd e))))

        // check for bingo
        let havingBingoRow = newBingoFields |> isBingo 
        let havingBingoColumn = newBingoFields |> Seq.map pivot |> isBingo
        // check for bingo
        match (havingBingoRow |> Seq.length) > 0, (havingBingoColumn |> Seq.length) > 0 with
        | true, _ ->
            let notCalledSum:int = sumUnmarkedBingoFields havingBingoRow
            printfn $"row bingo sum %d{notCalledSum} last number %d{int head} "
            notCalledSum * (int head)
        | _, true ->
            let notCalledSum:int = sumUnmarkedBingoFields havingBingoColumn
            printfn $"column bingo sum %d{notCalledSum} last number %d{int head} "
            notCalledSum * (int head)
        | false, false -> playBingo tail newBingoFields

    match numbers with
    | [] -> -1
    | [lastNumber] -> play lastNumber []
    | head::tail -> play head tail


let testInput = getTestInput 4
let testNumbers = testInput |> Seq.head |> fun s -> s.Split(",") |> Seq.toList
let testBingoFields = parseBingofields (testInput |> Array.skip 2) [||]

playBingo testNumbers testBingoFields |> printfn "Bingo Baby: %d"

let input = getInput 4
let numbers = input |> Seq.head |> fun s -> s.Split(",") |> Seq.toList
let bingoFields = parseBingofields (input |> Array.skip 2) [||]

playBingo numbers bingoFields |> printfn "Bingo Baby: %d"