#time
open System.IO
open System

let getTestInput day =
    let filename day = Path.Combine(__SOURCE_DIRECTORY__, $"Input/TestDay{day}.txt")
    File.ReadAllLines(filename day)

let getInput day =
    let filename day = Path.Combine(__SOURCE_DIRECTORY__, $"Input/Day{day}.txt")
    File.ReadAllLines(filename day)

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

let isLegalClose expected actual = expected = actual

let findIllegalCharacter (input:string) =
    let chars = (input.ToCharArray() |> Array.map (fun c -> c.ToString())) |> Seq.toList

    let rec findFault (openChars:string list) remainingChars =
        match remainingChars with
        | [] -> ""
        | [lastChar] ->
            if isOpeningChar lastChar then
                ""
            else
                let lastOpen = (openChars |> List.head)

                if isLegalClose (getClosingChar lastOpen) lastChar then
                    ""
                else
                    lastChar
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

// part 1
// getTestInput 10
getInput 10
|> Seq.map findIllegalCharacter
|> Seq.map getErrorPoints
|> Seq.sum