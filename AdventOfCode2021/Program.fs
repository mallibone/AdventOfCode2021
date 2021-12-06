open System.IO

let getTestInput day =
    let filename day = Path.Combine(__SOURCE_DIRECTORY__, $"Input/TestDay{day}.txt")
    File.ReadAllLines(filename day)

let getInput day =
    let filename day = Path.Combine(__SOURCE_DIRECTORY__, $"Input/Day{day}.txt")
    File.ReadAllLines(filename day)

printfn "Advent of Code 2021"

let printDay day dayFunc =
    printfn $"** Day {day} **"
    dayFunc (getTestInput day) (getInput day)

[(Day1.executeDay); (Day2.executeDay); (Day3.executeDay); (Day4.executeDay); (Day5.executeDay); (Day6.executeDay)]
|> List.iteri (fun i func -> printDay (i+1) func)
