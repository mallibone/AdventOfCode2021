module Day20

type FieldState = L | D | U

let charToFieldState c =
    match c with
    | '.' -> D
    | '#' -> L
    | _ -> U

let parseInput (input:string[]) =
    let enhancementAlgorithm = input[0]

    let originalImage =
        input 
        |> Array.skip 2
        |> Array.map (fun s -> s.ToCharArray() |> Array.map charToFieldState)
        |> array2D

    enhancementAlgorithm, originalImage

let getNeighbourCoords (x,y) =
    [(-1,-1);(0,-1);(1,-1);
        (-1,0);(0,0);(1,0);
        (-1,1);(0,1);(1,1);]
        |> Seq.map (fun (dx, dy) -> (dx+x,dy+y) )

let bitArrayToNumber (bitArray:seq<FieldState>) =
    // printfn "The binary string: %A" (String.Join("", bitArray))
    bitArray
    |> Seq.rev
    |> Seq.mapi (fun i c -> (i,c))
    |> Seq.fold (fun n (i,c) -> n + (if c = L then (2.0**(float i) |> int) else 0)) 0

let enlargeImage pixelsToAdd undefinedState (originalImage:FieldState[,]) =
    let orgX = originalImage |> Array2D.length2
    let orgY = originalImage |> Array2D.length1
    let enhancedImage = Array2D.init (orgY + pixelsToAdd) (orgX + pixelsToAdd) (fun _ _ -> undefinedState)
    
    let leftOffset = pixelsToAdd / 2
    let rightOffset = (pixelsToAdd / 2) - 1
    enhancedImage[leftOffset..(orgY+rightOffset),leftOffset..(orgX+rightOffset)] <- originalImage[0.. (orgY - 1),0 .. (orgX - 1)]
    enhancedImage

let processWithAlgorithm (algorithm:string) (inputValues:seq<FieldState>) =
    let offset = bitArrayToNumber inputValues
    algorithm[offset] |> charToFieldState

let enhanceImage algorithm padding (image:FieldState[,]) =
    let defaultValue = match image[1,1] with | L -> L | _ -> D
    let image = enlargeImage padding defaultValue image
    let enhancedImage = image |> Array2D.copy

    let maxX = (image |> Array2D.length2) - 1
    let maxY = (image |> Array2D.length1) - 1
    let processPixel = processWithAlgorithm algorithm

    for x in [0 .. maxX] do
        for y in [0 .. maxY] do
            enhancedImage[y,x] <- getNeighbourCoords (x, y) |> Seq.map (fun (xx,yy) -> if xx >= 1 && xx < maxX && yy < maxY && yy >= 1 then image[yy,xx] else defaultValue) |> processPixel
    enhancedImage

let countLights (image:FieldState[,]) =
    let maxX = (image |> Array2D.length2) - 1
    let maxY = (image |> Array2D.length1) - 1
    seq{for x in [0 .. maxX] do
        for y in [0 .. maxY] do
            yield image[y,x]}
    |> Seq.filter (fun c -> c = L)
    |> Seq.length

let part1 (input:string[]) =
    let enhancementAlgorithm, img = parseInput input
    let imageEnhancer  = enhanceImage enhancementAlgorithm

    img
    |> imageEnhancer 4
    |> imageEnhancer 2
    |> countLights

let part2 (input:string[]) =
    let enhancementAlgorithm, img = parseInput input
    let imageEnhancer  = enhanceImage enhancementAlgorithm

    let firstRunEnhancedImage =
        img
        |> imageEnhancer 4

    [0 .. 48]
    |> Seq.fold (fun state _ -> imageEnhancer 2 state) firstRunEnhancedImage
    |> countLights

let executeDay (testInput:string[]) (input:string[]) =
    // part 1
    testInput
    |> part1
    |> printfn "Part 1 Test: %A"

    input
    |> part1
    |> printfn "Part 1: %A"

    // part 2
    testInput
    |> part2
    |> printfn "Part 2 Test: %A"

    input
    |> part2
    |> printfn "Part 2: %A"