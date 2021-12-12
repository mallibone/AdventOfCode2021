module Day12

open System
open FSharp.Collections.ParallelSeq

let parseInput (input:string[]) =
    input |> Array.collect (fun s -> 
        let points = s.Split("-")
        [|(points[0], points[1]);(points[1], points[0])|])
    |> Array.groupBy fst
    |> Seq.map(fun route -> (fst route, (snd route) |> Seq.map snd |> Seq.toList))
    |> Map

let findDuplicates items = 
    items
    |> Seq.groupBy id
    |> Seq.choose(fun (key, values) ->
        if (values |> Seq.length) > 1 then Some key else None)

let hasDuplicates items = (findDuplicates items |> Seq.length) >= 1

let isOkayToVisitPoint (visitedPoints:list<string>) pointToVisit = 
    let lowerCasePoints = visitedPoints |> Seq.filter(fun p -> p.ToCharArray() |> Seq.forall Char.IsLower)
    lowerCasePoints |> Seq.contains pointToVisit |> not

let isOkayToVisitPointPart2 (visitedPoints:list<string>) pointToVisit = 
    let isLowerCase (ptv:string) = ptv[0] |> Char.IsLower
    let lowerCasePoints = visitedPoints |> List.filter(fun p -> p[0] |> Char.IsLower)

    match pointToVisit with
    | "start" -> false
    | ptv when (isLowerCase ptv) -> 
        (hasDuplicates lowerCasePoints |> not) || (lowerCasePoints |> Seq.exists (fun vp -> vp = ptv) |> not)
    | _ -> true

let findValidRoutePoints validPointFilter (routeMap:(Map<string,list<string>>)) (currentRoutes:list<list<string>>) =
    currentRoutes 
    |> List.collect(fun currentRoute ->
        let currentPoint = currentRoute |> Seq.head
        match currentPoint with
        | "end" -> [currentRoute]
        | _ -> 
            routeMap[currentPoint]
            |> List.filter (validPointFilter currentRoute)
            |> List.map (fun np -> (np::currentRoute))
            )
    |> PSeq.toList

let rec findRoutesPart1 currentPaths (routeMap:(Map<string,list<string>>)) =
    let newPaths = findValidRoutePoints isOkayToVisitPoint routeMap currentPaths
    // let openPaths = newPaths |> PSeq.filter(fun path -> (path |> List.head) <> "end") |> PSeq.toList
    // let closedPaths = newPaths |> PSeq.filter(fun path -> (path |> List.head) = "end") |> PSeq.toList
    if newPaths |> List.forall(fun path -> (path |> List.head) = "end") then
        newPaths
    else
        findRoutesPart1 newPaths (routeMap:(Map<string,list<string>>))

let rec findRoutesPart2 currentPaths (routeMap:(Map<string,list<string>>)) =
    let newPaths = findValidRoutePoints isOkayToVisitPointPart2 routeMap currentPaths
    if newPaths |> PSeq.forall(fun path -> (path |> List.head) = "end") then
        newPaths
    else
        findRoutesPart2 newPaths (routeMap:(Map<string,list<string>>)) 

let part1 (input:string[]) =
    input
    |> parseInput
    |> findRoutesPart1 [["start"]]
    |> List.length

let part2 (input:string[]) =
    input
    |> parseInput
    |> findRoutesPart2 [["start"]]
    |> List.length

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