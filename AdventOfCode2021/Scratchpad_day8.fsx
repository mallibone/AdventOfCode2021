#time
open System.IO
open System

let getTestInput day =
    let filename day = Path.Combine(__SOURCE_DIRECTORY__, $"Input/TestDay{day}.txt")
    File.ReadAllLines(filename day)

let getInput day =
    let filename day = Path.Combine(__SOURCE_DIRECTORY__, $"Input/Day{day}.txt")
    File.ReadAllLines(filename day)

let decode (input:string[]*string[]) =

    let uniqueInputs = fst input
    
    let getUniqueDigit i =
        match i with
        | 1 -> uniqueInputs |> Seq.find(fun s -> s.Length = 2)
        | 4 -> uniqueInputs |> Seq.find(fun s -> s.Length = 4)
        | 7 -> uniqueInputs |> Seq.find(fun s -> s.Length = 3)
        | 8 -> uniqueInputs |> Seq.find(fun s -> s.Length = 7)
        | _ -> ""

    let getDigit (decoder:seq<int*string>) i =
        let isNine (s:string) = (decoder |> Seq.find(fun d -> (fst d) = 4) |> snd).ToCharArray() |> Array.sort |> Seq.forall (fun c -> s.Contains(c))
        let isZero (s:string) = (decoder |> Seq.find(fun d -> (fst d) = 7) |> snd).ToCharArray() |> Array.sort |> Seq.forall (fun c -> s.Contains(c)) && (not (isNine s))
        match i with
        | 9 -> 
            uniqueInputs 
            |> Seq.filter(fun s -> s.Length = 6) 
            |> Seq.find isNine
        | 0 -> 
            uniqueInputs 
            |> Seq.filter(fun s -> s.Length = 6) 
            |> Seq.find isZero
        | _ -> ""

    let uniqueDecoder = [1;4;7;8;] |> Seq.map(fun i -> (i, (getUniqueDigit i)))
    // let fuzzyInputs = uniqueInputs |> Seq.filter(fun s -> not (uniqueDecoder |> Seq.exists(fun ud -> (snd ud) = s)))

    [9;0;6;2;3;5] |> Seq.map(fun i -> (i, (getDigit uniqueDecoder i)))
    |> Seq.append uniqueDecoder
    |> Seq.map (fun (digit, code) -> ((code.ToCharArray() |> Array.sort |> String), digit))
    |> dict
    // uniqueInputs |> Seq.filter(fun ui -> not ([1;4;7;9] |> Seq.contains ui))
        
    // // Map[("acedgfb", 8);("bcdef", 5);("acdfg", 2);("abcdf",3);("abd",7);("abcdef",9);("bcdefg",6);("abef",4);("abcdeg",0);("ab",1)]
    // decoder
    // [1;2;3]


// part 1
getTestInput 8
getInput 8
|> Seq.collect (fun s -> (s.Split("|")[1]).Trim(' ').Split(" "))
|> Seq.filter(fun digit -> digit.Length = 2 || digit.Length = 4 || digit.Length = 3 || digit.Length = 7) // 1, 4, 7, 8
|> Seq.length

// part 2
getTestInput 8
// getInput 8
|> Seq.map (fun s -> (s.Split("|") |> (fun segs -> (segs[0].Trim(' ').Split(" "), segs[1].Trim(' ').Split(" ")))))
|> Seq.collect decode
|> Seq.toList
|> printfn "%A"
// |> Seq.map(fun digit -> decoder[(digit.ToCharArray() |> Array.sort |> String)])
// |> Seq.filter(fun digit -> digit.Length = 2 || digit.Length = 4 || digit.Length = 3 || digit.Length = 7)
|> Seq.sum


// use the first part of the line to decode the numbers
// first parse the unique ones
// 0,6,9 -> 6 digits (9 must contain 4) (0 must contain 7 but not 4)
// 2,3,5 -> 5 digits -> (3 must contain 1), (2 is missing two chars variable 9)

// then decode the number as a result