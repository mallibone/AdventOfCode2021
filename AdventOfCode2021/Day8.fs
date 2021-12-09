module Day8

open System

let orderInput (input:string) = input.ToCharArray() |> Array.sort |> String

let getUniqueDigit (input:string) =
    // first parse the unique ones: 1, 4, 7, 8
    match input.Length with
    | 2 -> (1, orderInput input)
    | 4 -> (4, orderInput input)
    | 3 -> (7, orderInput input)
    | 7 -> (8, orderInput input)
    | _ -> (0, "")

// 0,6,9 -> 6 digits (9 must contain 4) (0 must contain 7 but not 4)
let get6SegmentDigits (decoder:seq<int*string>) (input:string) =
    let isNine (s:string) = (decoder |> Seq.find(fun d -> (fst d) = 4) |> snd).ToCharArray() |> Seq.forall (fun c -> s.Contains(c))
    let isZero (s:string) = (decoder |> Seq.find(fun d -> (fst d) = 7) |> snd).ToCharArray() |> Seq.forall (fun c -> s.Contains(c)) && (not (isNine s))

    match input with
    | nineInput when isNine nineInput -> (9, orderInput input)
    | zeroInput when isZero zeroInput -> (0, orderInput input)
    | _ -> (6, orderInput input)

// 2,3,5 -> 5 digits -> (3 must contain 1), (5 is missing one chars variable 6)
let get5SegmentDigits (decoder:seq<int*string>) (input:string) =
    let isThree (s:string) = (decoder |> Seq.find(fun d -> (fst d) = 1) |> snd).ToCharArray() |> Seq.forall (fun c -> s.Contains(c))
    let isFive (s:string) = (((decoder |> Seq.find(fun d -> (fst d) = 6) |> snd).ToCharArray() |> Seq.filter (fun c -> not (s.Contains(c))) |> Seq.length) = 1)

    match input with
    | threeInput when isThree threeInput -> (3, orderInput threeInput)
    | fiveInput when isFive fiveInput -> (5, orderInput input)
    | _ -> (2, orderInput input)

let decode (input:string[]*string[]) =

    let uniqueInputs = fst input

    let uniqueDecoder = uniqueInputs |> Seq.map getUniqueDigit |> Seq.filter (fun (n,_) -> n <> 0)
    let sixSegmentDecoders = uniqueInputs |> Seq.filter(fun ui -> ui.Length = 6) |> Seq.map (get6SegmentDigits uniqueDecoder)

    let partialDecoder = Seq.append uniqueDecoder sixSegmentDecoders

    let decoder =
        uniqueInputs 
        |> Seq.filter(fun ui -> ui.Length = 5) |> Seq.map (get5SegmentDigits partialDecoder)
        |> Seq.append partialDecoder
        |> Seq.sortBy fst
        |> Seq.map (fun (digit, code) -> ((code.ToCharArray() |> Array.sort |> String), digit))
        |> Seq.toList
        |> dict
    
    (snd input) |> Array.map orderInput |> Array.map(fun i -> decoder[i].ToString()) |> fun a -> String.Join("", a) |> fun s -> Convert.ToInt32(s)

// part 1
let part1 (input:string[])  = 
    input
    |> Seq.collect (fun s -> (s.Split("|")[1]).Trim(' ').Split(" "))
    |> Seq.filter(fun digit -> digit.Length = 2 || digit.Length = 4 || digit.Length = 3 || digit.Length = 7) // 1, 4, 7, 8
    |> Seq.length

// part 2
let part2 (input:string[]) = 
    input
    |> Seq.map (fun s -> (s.Split("|") |> (fun segs -> (segs[0].Trim(' ').Split(" "), segs[1].Trim(' ').Split(" ")))))
    |> Seq.map decode
    |> Seq.sum

let executeDay (testInput:string[]) (input:string[]) =
    // part 1
    testInput
    |> part1
    |> printfn "Part 1 Test: %d"

    input
    |> part1
    |> printfn "Part 1: %d"

    // part 2
    testInput
    |> part2
    |> printfn "Part 2 Test: %d"

    input
    |> part2
    |> printfn "Part 2: %d"