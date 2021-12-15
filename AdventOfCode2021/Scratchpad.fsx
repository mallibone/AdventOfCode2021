#time
open System.IO
open System

let getTestInput day =
    let filename day = Path.Combine(__SOURCE_DIRECTORY__, $"Input/TestDay{day}.txt")
    File.ReadAllLines(filename day)

let getInput day =
    let filename day = Path.Combine(__SOURCE_DIRECTORY__, $"Input/Day{day}.txt")
    File.ReadAllLines(filename day)

let parseInput (inputLines:string[]) =
    let initialInput = inputLines[0]

    let ruleSet =
        inputLines 
        |> Array.skip 2
        |> Array.map (fun l -> 
            let parts = l.Split(" -> ")
            parts[0], parts[1])
        |> Map.ofArray
    initialInput, ruleSet

let getNewElement (ruleset:Map<string,string>) (pairs:char[]) =
    let key = String.Join("", pairs)
    $"{pairs[0]}{ruleset[key]}"

let part1 input =
    let (template, ruleSet) = parseInput input
    let getNewElementFromRuleset = getNewElement ruleSet

    let rec runProcess processCount (processInput:string) =
        if processCount > 0 then
            processInput.ToCharArray() 
            |> Array.windowed 2
            |> Array.map getNewElementFromRuleset
            |> fun pairs -> $"""{String.Join("", pairs)}{template[template.Length - 1]}"""
            |> runProcess (processCount - 1)
        else
            processInput

    runProcess 10 template
    |> (fun s -> s.ToCharArray())
    |> Array.groupBy id
    |> Array.map (fun (k, v) -> (k, v |> Array.length))
    |> Array.sortByDescending snd
    |> fun sorted -> (sorted |> Array.head |> snd) - (sorted |> Array.last |> snd)

let getNextStep (ruleSet:Map<string,string>)(processInput:(string*int64)[]) =
    let nextInput = processInput 
                    |> Seq.collect (fun (k,v) -> 
                        // let chars = k.ToCharArray()
                        [|($"{k[0]}{ruleSet[k]}", v);($"{ruleSet[k]}{k[1]}", v)|])

                    |> Seq.groupBy fst
                    |> Seq.map (fun (k, v) -> (k, v |> Seq.sumBy snd))
                    |> Seq.toArray
    nextInput

let part2 input =
    let (template, ruleSet) = parseInput input
    let getNewElementFromRuleset = getNewElement ruleSet

    let runProcessWithRuleSet = getNextStep ruleSet

    let pairs =
        template.ToCharArray() 
        |> Array.windowed 2
        |> Array.map(fun k -> String.Join("", k), 1L)

    let rec runProcess processCount (processInput:(string*int64)[]) =
        if processCount > 0 then
            let newProcessInput = 
                processInput
                |> runProcessWithRuleSet
            runProcess (processCount - 1) newProcessInput
        else
            processInput

    runProcess 40 pairs
    |> Array.map (fun (key, count) -> (key[0], count))
    |> Array.groupBy fst
    |> Array.map (fun (k, kv) -> (k, (kv |> Array.sumBy snd) + if k = template[(template.Length - 1)] then 1L else 0L))
    |> Array.sortByDescending snd
    |> fun sorted -> (snd sorted[0]) - (snd sorted[sorted.Length - 1])


getTestInput 14
|> part1
getInput 14
|> part1
getInput 14
|> part2