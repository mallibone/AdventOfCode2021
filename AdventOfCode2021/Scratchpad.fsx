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
    let getPosition (line:string) = line.Split(": ")[1] |> int
    (input[0] |> getPosition) - 1, (input[1] |> getPosition) - 1

type GameScore = {DiceRolls:int;Player1Score:int;Player2Score:int}
let part1 (input:string[]) =
    let posPlayer1, posPlayer2 = parseInput input
    let playFieldSize = 10
    let winningScore = 1000
    let rec playRound diceRolls round diceRollCount pos1 score1 pos2 score2 =
        if score1 >= winningScore || score2 >= winningScore then
            {DiceRolls = diceRollCount; Player1Score = score1; Player2Score = score2}
        else
            if round % 2 = 1 then
                let player1Field = (pos1 + (diceRolls |> Seq.take 3 |> Seq.sum)) % playFieldSize
                playRound (diceRolls |> Seq.skip 3) (round + 1) (diceRollCount + 3) player1Field (score1 + player1Field + 1) pos2 score2
            else
                let player2Field = (pos2 + (diceRolls |> Seq.take 3 |> Seq.sum)) % playFieldSize
                playRound (diceRolls |> Seq.skip 3) (round + 1) (diceRollCount + 3) pos1 score1 player2Field (score2 + player2Field + 1)
    playRound [1 .. 1000] 1 0 posPlayer1 0 posPlayer2 0
    |> fun res -> res.DiceRolls * if res.Player1Score > res.Player2Score then res.Player2Score else res.Player1Score

getTestInput 21
getInput 21
|> part1
 