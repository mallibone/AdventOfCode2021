module Day4

open System

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
        // printfn "input left: %A" input
        parseBingofields (input |> Seq.skip (getSkip input)) bingoFields

let isBingo bingoBoardFields =
    bingoBoardFields |> Seq.exists (Seq.forall (fun x -> (snd x) = 1))

let sumUnmarkedBingoFields fields =
    fields |> Seq.sumBy (fun bingoFields -> bingoFields |> Seq.collect(fun bingoRowFields -> bingoRowFields |> Seq.filter(fun field -> (snd field) = 0)) |> Seq.sumBy(fun field -> int (fst field)))

let playBingoRound roundNumber bingoFields =
    bingoFields
    |> Seq.map (fun field -> field |> Seq.map (fun lineItems -> lineItems |> Seq.map(fun e -> if fst e = roundNumber then (fst e, (snd e) + 1) else (fst e, snd e))))

let rec playBingoPart1 (numbers:list<string>) bingoFields =
    match numbers with
    | [] -> -1
    | head::tail -> 
        let newBingoFields = playBingoRound head bingoFields
        // check for bingo
        let havingBingoRow = (newBingoFields |> Seq.filter isBingo) |> Seq.append (newBingoFields |> Seq.map pivot |> Seq.filter isBingo)
        // check for bingo
        if (havingBingoRow |> Seq.length) > 0 then
            let notCalledSum:int = sumUnmarkedBingoFields havingBingoRow
            // printfn $"row bingo sum %d{notCalledSum} last number %d{int head} "
            notCalledSum * (int head)
        else playBingoPart1 tail newBingoFields

let rec playBingoPart2 (numbers:list<string>) bingoFields =
    match numbers with
    | [] -> -1
    | head::tail -> 
        let newBingoFields = 
            playBingoRound head bingoFields
            |> Seq.filter (fun x -> not (isBingo x))
            |> Seq.filter (fun x -> x |> pivot |> isBingo |> not )
        if newBingoFields |> Seq.length = 1 then
            playBingoPart1 tail newBingoFields
        else
            playBingoPart2 tail newBingoFields


let executeDay testInput input =
    let testNumbers = testInput |> Seq.head |> fun (s:string) -> s.Split(",") |> Seq.toList
    let testBingoFields = parseBingofields (testInput |> Array.skip 2) [||]

    let numbers = input |> Seq.head |> fun (s:string) -> s.Split(",") |> Seq.toList
    let bingoFields = parseBingofields (input |> Array.skip 2) [||]

    playBingoPart1 testNumbers testBingoFields |> printfn "Part1 Test Bingo Baby: %d"
    playBingoPart1 numbers bingoFields |> printfn "Part1 Bingo Baby: %d"

    playBingoPart2 testNumbers testBingoFields |> printfn "Part2 Test Bingo Baby: %d"
    // playBingoPart2 numbers bingoFields |> printfn "Part2 Bingo Baby: %d"