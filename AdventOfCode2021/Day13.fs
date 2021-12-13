module Day13

open System

let flat2Darray array2D = 
    seq { for x in [0..(Array2D.length1 array2D) - 1] do 
                for y in [0..(Array2D.length2 array2D) - 1] do 
                    yield array2D.[x, y] }

let toJagged<'a> (arr: 'a[,]) : 'a [][] = 
    [| for x in 0 .. Array2D.length1 arr - 1 do
            yield [| for y in 0 .. Array2D.length2 arr - 1 -> arr.[x, y] |]
        |]

let parseCoordinates (coordinateString:string) =
    let coords = coordinateString.Split(",")
    (Convert.ToInt32 coords[1],Convert.ToInt32 coords[0])

let parseInstruction (instructionString:string) =
    let instructionParts = instructionString.Split("=")
    (instructionParts[0].ToCharArray() |> Array.last |> fun c -> c.ToString(), instructionParts[1] |> int)

let parseInput (inputRows:string[]) =
    let inputSplitIndex = inputRows |> Array.findIndex (fun s -> s = "")
    
    let coordiantes =
        inputRows |> Array.take (inputSplitIndex)
        |> Array.map parseCoordinates
    
    let foldingInstructions =
        inputRows |> Array.skip (inputSplitIndex + 1)
        |> Array.map parseInstruction

    (coordiantes, foldingInstructions)

let horizontalFold index (matrix:string[,]) =
    let createNewMatrix maxX maxY =
        Array2D.init maxY maxX (fun _ _ -> ".") 

    let leftHalf =
        let maxX = (index)
        let maxY = matrix |> Array2D.length1
        let newMatrix = createNewMatrix maxX maxY
        newMatrix[0..maxY-1,0..maxX-1] <- matrix
        newMatrix

    let rightHalf =
        let midPointOffset = index + 1
        let maxY = matrix |> Array2D.length1
        let maxX = (matrix |> Array2D.length2) - midPointOffset
        let newMatrix = createNewMatrix maxX maxY
        newMatrix[0..maxY-1,0..maxX-1] <- matrix[0 .. (maxY - 1),midPointOffset .. ((matrix |> Array2D.length2) - 1)]
        newMatrix
    
    let halfMaxX = (rightHalf |> Array2D.length2)
    let maxY = (rightHalf |> Array2D.length1)
    let maxX = (rightHalf |> Array2D.length2)
    for y in 0 .. (maxY - 1) do
        for x in 0 .. (maxX - 1) do
            let rightHalfValue = rightHalf[y, (halfMaxX - 1) - x]
            if rightHalfValue = "#" then
                if leftHalf |> Array2D.length2 > halfMaxX then
                    leftHalf[y,x+1] <- rightHalfValue
                else
                    leftHalf[y,x] <- rightHalfValue

    leftHalf

let verticalFold index (matrix:string[,]) =
    let createNewMatrix  maxX maxY =
        Array2D.init maxY maxX (fun _ _ -> ".") 

    let upperHalf =
        let maxX = matrix |> Array2D.length2
        let maxY = (index)
        let newMatrix = createNewMatrix maxX maxY
        newMatrix[0..maxY-1,0..maxX-1] <- matrix
        newMatrix

    let bottomHalf =
        let midPointOffset = (index + 1)
        let maxX = matrix |> Array2D.length2
        let maxY = (((matrix |> Array2D.length1))-midPointOffset)
        let newMatrix = createNewMatrix maxX maxY

        newMatrix[0..(maxY - 1),0..(maxX - 1)] <- matrix[midPointOffset .. ((matrix |> Array2D.length1) - 1), 0 .. (maxX - 1)]
        newMatrix
    
    let halfMaxY = (bottomHalf |> Array2D.length1)
    let maxY = (bottomHalf |> Array2D.length1)
    let maxX = (bottomHalf |> Array2D.length2)
    for y in 0 .. (maxY - 1) do
        for x in 0 .. (maxX - 1) do
            let bottomHalfValue = bottomHalf[(halfMaxY - 1) - y, x]
            if bottomHalfValue = "#" then 
                if upperHalf |> Array2D.length1 > halfMaxY then
                    upperHalf[y+1,x] <- bottomHalfValue
                else
                    upperHalf[y,x] <- bottomHalfValue

    upperHalf

let fold matrix (instruction:(string*int)) =
    let instructionToExecute =
        match fst instruction with
        | "x" -> horizontalFold (snd instruction)
        | "y" -> verticalFold (snd instruction)
        | _ -> raise (NotImplementedException($"Unkown fold instruction {fst instruction}"))

    instructionToExecute matrix

let doTheFolding ((coordinates:(int*int)[]), (foldInstructions:(string*int)[])) =
    let maxY = (coordinates |> Seq.map fst |> Seq.max) + 1
    let maxX = (coordinates |> Seq.map snd |> Seq.max) + 1
    let initialPaper = Array2D.init maxY maxX (fun _ _ -> ".")
    coordinates |> Array.iter(fun (x, y) -> initialPaper[x,y] <- "#")
    foldInstructions |> Array.fold fold initialPaper

let part1 input =
    input
    |> parseInput
    |> fun (coords, instructions) -> doTheFolding (coords, ([|instructions |> Array.head|]))
    |> flat2Darray
    |> Seq.filter (fun v -> v = "#")
    |> Seq.length

let part2 input =
    input
    |> parseInput
    |> doTheFolding
    |> toJagged
    |> Array.map(fun s -> String.Join("", s))
    |> (fun s -> String.Join("\n", s))

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
    |> printfn "Part 2 Test:\n\n%s"

    input
    |> part2
    |> printfn "Part 2:\n\n%s"