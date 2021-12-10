module Day10

open FSharp.Collections.ParallelSeq

let isOpeningChar char =
    match char with
    | "(" -> true
    | "[" -> true
    | "<" -> true
    | "{" -> true
    | _ -> false

let getClosingChar char =
    match char with
    | "(" -> ")"
    | "[" ->  "]"
    | "<" -> ">"
    | "{" -> "}"
    | _ -> ""

let getErrorPoints char =
    match char with
    | ")" -> 3
    | "]" -> 57
    | "}" -> 1197
    | ">" -> 25137
    | _ -> 0

let getCompletionScore char =
    match char with
    | ")" -> 1L
    | "]" -> 2L
    | "}" -> 3L
    | ">" -> 4L
    | _ -> 
        printfn "this is odd..."
        0L

let isLegalClose expected actual = expected = actual

let getElementsFromLine (input:string) = (input.ToCharArray() |> Array.map (fun c -> c.ToString())) |> Seq.toList

let findIllegalCharacter (input:string) =
    let chars = getElementsFromLine input

    let rec findFault (openChars:string list) remainingChars =
        match remainingChars with
        | [] -> ""
        | currentChar::restChars -> 
            if isOpeningChar currentChar then
                findFault (currentChar::openChars) restChars
            else
                let lastOpen = (openChars |> List.head)

                if isLegalClose (getClosingChar lastOpen) currentChar then
                    findFault (openChars |> List.tail) restChars
                else
                    currentChar
    findFault [] chars 

let filterIllegalLines (input:string) =
    match findIllegalCharacter input with
    | "" -> true
    | _ -> false

let getMissingBraces (input:string) =
    let chars = getElementsFromLine input

    let rec findClosingChars (openChars:string list) remainingChars =
        match remainingChars with
        | [] -> [""]
        | [lastChar] ->
            if isOpeningChar lastChar then
                (lastChar::openChars) |> List.map getClosingChar
            else
                openChars |> Seq.tail |> Seq.map getClosingChar |> Seq.toList
        | currentChar::restChars -> 
            if isOpeningChar currentChar then
                findClosingChars (currentChar::openChars) restChars
            else
                let remainingOpen = (openChars |> List.tail)
                findClosingChars remainingOpen restChars
    findClosingChars [] chars

let part1 (input:string[]) =
    input
    |> PSeq.map findIllegalCharacter
    |> Seq.map getErrorPoints
    |> Seq.sum

let part2 (input:string[]) =
    let scores =
        input
        |> PSeq.filter filterIllegalLines 
        |> Seq.map getMissingBraces
        |> Seq.map (fun b -> b |> Seq.fold (fun score elem -> (score * 5L) + (getCompletionScore elem)) 0L)
        |> Seq.sort
        |> Seq.toList

    scores[scores.Length/2]

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