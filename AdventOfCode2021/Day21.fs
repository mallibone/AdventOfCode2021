module Day21

open FSharp.Collections.ParallelSeq

let parseInput (input:string[]) =
    let getPosition (line:string) = line.Split(": ")[1] |> int
    (input[0] |> getPosition) - 1, (input[1] |> getPosition) - 1

type GameScore = {DiceRolls:int;Player1Score:int;Player2Score:int}
let playFieldSize = 10
let part1 (input:string[]) =
    let posPlayer1, posPlayer2 = parseInput input
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

type Turn = 
    | Player1 
    | Player2
type Player = {Position:int; Score:int}
type Game = {Player1:Player;Player2:Player;Turn:Turn;Winner:int;}
type Score = {Player1Wins:int64;Player2Wins:int64}

let diceRols = 
    [
        for i in [1 .. 3] do
            for ii in [1 .. 3] do
                for iii in [1 .. 3] do
                yield i + ii + iii
    ] 
    |> List.groupBy id
    |> List.map (fun (k,v) -> (k, int64 ((v |> List.length))))

// let roleDice winningScore (game:KeyValuePair<Game,int>) =
let roleDice winningScore (game:Game, gameCount:int64) =
    let movePlayer turn (moveBy, newGames) =
        match turn with
        | Player1 -> 
            let newPosition = (game.Player1.Position + moveBy) % playFieldSize
            let newScore = game.Player1.Score + (newPosition + 1)
            {game with Player1 = {Position = newPosition; Score = newScore}; Turn = Player2; Winner = if newScore >= winningScore then 1 else 0}, gameCount * newGames
        | Player2 ->
            let newPosition = (game.Player2.Position + moveBy) % playFieldSize
            let newScore = game.Player2.Score + (newPosition + 1)
            {game with Player2 = {Position = newPosition; Score = newScore}; Turn = Player1; Winner = if newScore >= winningScore then 2 else 0}, gameCount * newGames
    diceRols
    |> Seq.map (movePlayer game.Turn)

let addGameToMap games (game:Game, gameCount:int64) =
    games |> Map.change game (fun ov -> 
        match ov with
        | Some v -> Some (v + gameCount)
        | _ -> Some gameCount)

let part2 (input:string[]) =
    let posPlayer1, posPlayer2 = parseInput input
    let winningScore = 21

    let initialGame = Map [{Player1 = {Position = posPlayer1; Score = 0}; Player2 = {Position = posPlayer2; Score = 0}; Turn = Player1; Winner = 0},1L]
    let initialScore = {Player1Wins = 0L; Player2Wins = 0L}

    let rec playRound openGames score =
        match openGames |> Map.keys |> Seq.toList with
        | [] -> score
        | _ ->
            let openGames, wonGames =
                openGames
                |> Seq.collect (fun kvp -> roleDice winningScore (kvp.Key, kvp.Value))
                |> PSeq.fold (fun (openGames, wonGames) game -> if (fst game).Winner > 0 then openGames, game::wonGames else (addGameToMap openGames game), wonGames) (Map<Game,int64>[],[])
                // |> PSeq.fold (fun (openGames, wonGames) game -> if (fst game).Winner > 0 then openGames, game::wonGames else game::openGames, wonGames) (Map<Game,int>[],[])
            // printfn $"Open Games {openGames |> Seq.sumBy (fun kvp -> kvp.Value)} / Won Games {wonGames |> Seq.sumBy snd}"
            let newScore = wonGames |> PSeq.fold (fun state game -> if (fst game).Winner = 1 then {state with Player1Wins = state.Player1Wins + (int64 (snd game))} else {state with Player2Wins = state.Player2Wins + (int64 (snd game))}) score
            playRound openGames newScore
    playRound initialGame initialScore

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