#time
open System.IO
open System

let getTestInput day =
    let filename day = Path.Combine(__SOURCE_DIRECTORY__, $"Input/TestDay{day}.txt")
    File.ReadAllLines(filename day)

let getInput day =
    let filename day = Path.Combine(__SOURCE_DIRECTORY__, $"Input/Day{day}.txt")
    File.ReadAllLines(filename day)

let parseInput (input:string[]) =
    input |> Array.collect (fun s -> 
        let points = s.Split("-")
        [|(points[0], points[1]);(points[1], points[0])|])
    |> Array.groupBy fst
    |> Seq.map(fun route -> (fst route, (snd route) |> Seq.map snd |> Seq.toList))
    |> Map

let isOkayToVisitPoint (visitedPoints:list<string>) pointToVisit = 
    let lowerCasePoints = visitedPoints |> Seq.filter(fun p -> p.ToCharArray() |> Seq.forall Char.IsLower)
    lowerCasePoints |> Seq.contains pointToVisit |> not

// let isOkayToVisitPointPart2 (visitedPoints:list<string>) pointToVisit = 
//     let isLowerCase ptv = ptv.ToCharArray() |> Seq.forall Char.IsLower

//     let lowerCasePoints = visitedPoints |> Seq.filter(fun p -> p.ToCharArray() |> Seq.forall Char.IsLower)
//     match pointToVisit with
//     | "start" -> false
//     | isLowerCase -> lowerCasePoints |> Seq.filter pointToVisit |> not
//     | _ -> true

let findValidRoutePoints (routeMap:(Map<string,list<string>>)) (currentRoutes:list<list<string>>) =
    currentRoutes |> List.collect(fun currentRoute ->
        let currentPoint = currentRoute |> Seq.head
        match currentPoint with
        | "end" -> [currentRoute]
        | _ -> 
            // printfn "%A" currentPoint
            let gna = 
                routeMap[currentPoint]
                |> List.filter (isOkayToVisitPoint currentRoute)
                |> List.map (fun np -> (np::currentRoute))
            gna)

let startJourney (routeMap:(Map<string,list<string>>)) =

    // let rec doStep validRoutes visitedPoints pointsToVisit =
    //     let nextRoutePoints = pointsToVisit |> List.map (findValidRoutes routeMap visitedPoints)
    //     let foundValidRoutes = nextRoutePoints |> List.filter (fun rp -> ((fst rp) |> Seq.length > 0) && ((snd rp) |> Seq.length = 0)) |> List.map fst
    //     let newValidRoutes = validRoutes@foundValidRoutes
    //     let nextSteps = nextRoutePoints |> List.filter (fun rp -> ((snd rp) |> Seq.length = 0))
    //     nextSteps |> List.collect (fun input -> doStep newValidRoutes input)

    let rec gna currentPaths =
        let newPaths = findValidRoutePoints routeMap currentPaths
        // printfn "%A" newPaths
        if newPaths |> List.forall(fun path -> (path |> List.head) = "end") then
            newPaths
        else
            gna newPaths

    gna [["start"]]



// getTestInput 12
getInput 12
|> parseInput
|> startJourney
|> List.length