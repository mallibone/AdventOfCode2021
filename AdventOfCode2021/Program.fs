open System.IO
open System.Diagnostics

let getTestInput day =
    let filename day = Path.Combine(__SOURCE_DIRECTORY__, $"Input/TestDay{day}.txt")
    File.ReadAllLines(filename day)

let getInput day =
    let filename day = Path.Combine(__SOURCE_DIRECTORY__, $"Input/Day{day}.txt")
    File.ReadAllLines(filename day)

printfn "Advent of Code 2021"

let printDay day dayFunc =
    let sw = new Stopwatch()
    sw.Start()
    printfn $"**** Day {day} ****"
    dayFunc (getTestInput day) (getInput day)
    sw.Stop()
    printfn $"Duration {sw.ElapsedMilliseconds} ms"
    printfn $"***************"

[(Day1.executeDay); (Day2.executeDay); (Day3.executeDay); (Day4.executeDay); (Day5.executeDay); (Day6.executeDay); 
    (Day7.executeDay); (Day8.executeDay); (Day9.executeDay); (Day10.executeDay); (Day11.executeDay); (Day12.executeDay);
    (Day13.executeDay)]
|> List.iteri (fun i func -> printDay (i+1) func)
