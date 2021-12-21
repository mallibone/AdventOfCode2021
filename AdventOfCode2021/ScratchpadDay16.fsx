#time
open System.IO
open System

let getTestInput day =
    let filename day = Path.Combine(__SOURCE_DIRECTORY__, $"Input/TestDay{day}.txt")
    File.ReadAllLines(filename day)

let getInput day =
    let filename day = Path.Combine(__SOURCE_DIRECTORY__, $"Input/Day{day}.txt")
    File.ReadAllLines(filename day)

type SnailFishPairValue =
    | Value of int
    | NestedPair of SnailFishPairValue

type SnailFishPair = {x : SnailFishPairValue; y : SnailFishPairValue}

let parseLine (inputLine:string) =
    let input = inputLine.ToCharArray() |> Seq.toList

    let rec parseInputToSnailFishPairs (parsedInput:SnailFishPair) inputChars =
        match inputChars with
        | [] -> parsedInput
        | '['::leftChars -> parsedInput.x = SnailFishPairValue
        | ','::leftChars -> 
        | ']'::leftChars -> // complete a pair....
        | n::leftChars ->




let part1 (input:string[]) =
    input
    |> Array.map parseLine

getTestInput 18
|> part1