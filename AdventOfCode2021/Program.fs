open System.IO

let getTestInput day =
    let filename day = Path.Combine(__SOURCE_DIRECTORY__, $"Input/TestDay{day}.txt")
    File.ReadAllLines(filename day)

let getInput day =
    let filename day = Path.Combine(__SOURCE_DIRECTORY__, $"Input/Day{day}.txt")
    File.ReadAllLines(filename day)

// For more information see https://aka.ms/fsharp-console-apps
// printfn "Hello from F#"
printfn "Advent of Code 2021"

let testInput = getTestInput 1
let input = getInput 1
Day1.executeDay1 testInput input