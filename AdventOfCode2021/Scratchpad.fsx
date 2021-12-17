#time
open System.IO
open System

let getTestInput day =
    let filename day = Path.Combine(__SOURCE_DIRECTORY__, $"Input/TestDay{day}.txt")
    File.ReadAllLines(filename day)

let getInput day =
    let filename day = Path.Combine(__SOURCE_DIRECTORY__, $"Input/Day{day}.txt")
    File.ReadAllLines(filename day)

type ValuePaket = {version:int64;paketType:int64;value:int64}
type OperatorPaket = 
    {version:int64;paketType:int64;subPaket:list<Package>}
and Package =
    | ValuePaket of ValuePaket
    | OperatorPaket of OperatorPaket

let hexToBinaryMap =
    [   ("0","0000")
        ("1","0001")
        ("2","0010")
        ("3","0011")
        ("4","0100")
        ("5","0101")
        ("6","0110")
        ("7","0111")
        ("8","1000")
        ("9","1001")
        ("A","1010")
        ("B","1011")
        ("C","1100")
        ("D","1101")
        ("E","1110")
        ("F","1111")] |> Map

let bitArrayToNumber (bitArray:seq<char>) =
    bitArray
    |> Seq.rev
    |> Seq.mapi (fun i c -> (i,c))
    |> Seq.fold (fun n (i,c) -> n + (if c = '1' then (2.0**(float i) |> int64) else 0)) 0L

let toBitArray (hexInput:string) =
    hexInput.ToCharArray()
    |> Array.map(fun c -> hexToBinaryMap[c.ToString()])
    |> fun binaries -> String.Join("", binaries).ToCharArray()

let parsePaket (inputStream:seq<char>) =
    let packageVersion = inputStream |> Seq.take 3 |> bitArrayToNumber
    let packageType = inputStream |> Seq.skip 3 |> Seq.take 3 |> bitArrayToNumber

    let remainingStream = inputStream |> Seq.skip 6
    (packageVersion, packageType, remainingStream)

let parseValuePaket (inputStream:seq<char>) =
    let rec getPakets (subPackages:list<char>) (stream:list<char>) =
        // printfn "Get value  paket"
        let subPackage = stream |> Seq.skip 1 |> Seq.take 4 |> Seq.rev |> Seq.toList
        if stream[0] = '0' then
            let valuePackage = subPackage@subPackages |> List.rev
            // printfn $"value package: {valuePackage}"
            let remainingStream = (stream |> List.skip (Math.Min(5, stream.Length)))
            // printfn $"remaining stream: {remainingStream}"
            valuePackage, remainingStream
        else
            let newSubPackages = subPackage@subPackages
            getPakets newSubPackages (stream |> List.skip 5)

    getPakets [] (inputStream |> Seq.toList)

let rec parsePakets (inputStream:seq<char>) : Package*seq<char> =
    let version, paketType, remainingStream = parsePaket inputStream
    // printfn "inputStream %A" inputStream
    // printfn $"version {version}"
    // printfn $"paketType {paketType}"
    match paketType with
    | 4L -> 
        let valuePaket, remainingStream = parseValuePaket remainingStream 
        (ValuePaket({version = version; paketType = paketType; value = valuePaket |> bitArrayToNumber}), remainingStream)
    | _ -> 
        let pakets, remainingStream = parseOperatorPaket (remainingStream |> Seq.toList)
        (OperatorPaket({ version = version; paketType = paketType; subPaket = pakets}), remainingStream)

and parseOperatorPaket (inputStream:list<char>) =
    // printfn "input stream %A" inputStream
    let lenghtTypeId = if inputStream[0] = '0' then 15 else 11
    // printfn $"Size Type: {lenghtTypeId}"
    let paketSize = inputStream |> Seq.skip 1 |> Seq.take lenghtTypeId |> bitArrayToNumber |> int
    // printfn $"Paket size: {paketSize}"

    let inputStream = inputStream |> Seq.skip (1 + lenghtTypeId) |> Seq.toList

    if lenghtTypeId = 15 then
        // take n bits
        let innerPaketStream = inputStream |> Seq.take paketSize |> Seq.toList
        let inputStream = inputStream |> Seq.skip paketSize |> Seq.toList
        let rec parseInnerPakets innerPakets (stream:seq<char>) =
            // printfn $"stream length: {stream |> Seq.length} original stream length: {inputStream |> Seq.length} paket size {paketSize}"
            if (innerPaketStream |> Seq.length) - (stream |> Seq.length) = paketSize then
                innerPakets |> List.rev, stream |> Seq.append inputStream
            else
                let paket, remainingStream = parsePakets stream
                parseInnerPakets (paket::innerPakets) remainingStream
        // parse paket until no more bits are left (trailing 0)
        // [1 .. (paketSize/11)] |> Seq.mapi (fun i _ -> if i+1 = (paketSize/11) then 11 + (paketSize % 11) else 11)
        parseInnerPakets [] innerPaketStream
    else 
        // parse n pakets out of remaining stream
        // [ 1 .. paketSize ] |> Seq.map (fun _ -> 11)
        let rec parseInnerPakets innerPakets (stream:seq<char>) =
            // printfn $"remaining stream: {stream}"
            // printfn $"inner pakets: {innerPakets |> Seq.length} paket size {paketSize}"
            if innerPakets |> Seq.length = paketSize then
                // printfn "done"
                innerPakets |> List.rev, stream
            else
                let paket, remainingStream = parsePakets stream
                parseInnerPakets (paket::innerPakets) remainingStream
        // parse paket until no more bits are left (trailing 0)
        // [1 .. (paketSize/11)] |> Seq.mapi (fun i _ -> if i+1 = (paketSize/11) then 11 + (paketSize % 11) else 11)
        parseInnerPakets [] inputStream

let rec sumVersions paket =
    match paket with
    | ValuePaket vp -> vp.version
    | OperatorPaket op -> op.version + (op.subPaket |> Seq.sumBy (fun p -> sumVersions p))

let rec calculatePackages paket =
    match paket with
    | ValuePaket vp -> vp.value
    | OperatorPaket op -> 
        match int op.paketType with
        | 0 -> op.subPaket |> Seq.sumBy calculatePackages
        | 1 -> op.subPaket |> Seq.fold (fun v p -> v * (calculatePackages p)) 1
        | 2 -> op.subPaket |> Seq.map calculatePackages |> Seq.min
        | 3 -> op.subPaket |> Seq.map calculatePackages |> Seq.max
        | 5 -> 
            if calculatePackages op.subPaket[0] > calculatePackages op.subPaket[1] then
                1
            else
                0
        | 6 -> 
            if calculatePackages op.subPaket[0] < calculatePackages op.subPaket[1] then
                1
            else
                0
        | 7 -> 
            if calculatePackages op.subPaket[0] = calculatePackages op.subPaket[1] then
                1
            else
                0
        | _ -> raise (new NotImplementedException($"Unkown type {op.paketType}"))

        // op.version + (op.subPaket |> Seq.sumBy (fun p -> sumVersions p))

let part1 (inputStream:string[]) =
    inputStream
    |> Seq.map toBitArray
    |> Seq.map parsePakets
    |> Seq.map fst
    |> Seq.map sumVersions

let part2 (inputStream:string[]) =
    inputStream
    |> Seq.map toBitArray
    |> Seq.map parsePakets
    |> Seq.map fst
    |> Seq.map calculatePackages
    |> Seq.toList

getTestInput 16
getInput 16
|> part2
// |> Array.map toBitArray
// |> Array.map parsePakets
// |> Array.map sumVersions