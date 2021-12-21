#time
open System.IO
open System
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Collections

let getTestInput day =
    let filename day = Path.Combine(__SOURCE_DIRECTORY__, $"Input/TestDay{day}.txt")
    File.ReadAllLines(filename day)

let getInput day =
    let filename day = Path.Combine(__SOURCE_DIRECTORY__, $"Input/Day{day}.txt")
    File.ReadAllLines(filename day)

let getMap (input:string[]) =
    input
    |> Array.map (fun s -> s.ToCharArray() |> Array.map (fun c -> Convert.ToInt32(c.ToString())))
    |> array2D

let parseCoordinates (coordStrings:List<string>) =
    coordStrings |> List.map(fun s -> s.Split(",") |> Seq.map int |> Seq.toList)

let parseBeacons (input:string[]) =
    let rec innerParse scanners  input =
        match input with
        | [||] -> scanners
        | _ ->
            let beaconReadings = input |> Seq.skip 1 |> Seq.takeWhile (fun s -> not (s = "")) |> Seq.toList
            innerParse (beaconReadings::scanners) (input |> Array.skip (Math.Min(input.Length, beaconReadings.Length + 2)))
    innerParse [] input

let splitScannerReadings (input:string[]) =
    parseBeacons input
    |> List.rev
    |> List.map parseCoordinates


let part1 (input:string[]) =
    input
    |> splitScannerReadings

getTestInput 19
|> part1