
{-# LANGUAGE LambdaCase #-}

module Main where

import Representations
import Generate
import Solution

import System.IO

commandGenerate::[String] -> IO Kogame
commandGenerate cs = do
    if null cs then
        generate 20 13
    else
        generate (read (head cs) :: Int) (read (cs!!1) :: Int)

commandReset::Kogame -> String -> IO Kogame
commandReset game file = do
    if file == "" then do
        putStrLn "You never load a map, please run load before reset"
        return game
    else do
        newKogame <- commandLoad file
        if newKogame == invalidGame then do
            putStrLn "Reset failed, the map file is invalid"
            return game
        else do
            putStrLn "Reset successfully!!"
            putStrLn "Your current map is:"
            print newKogame
            return newKogame

commandSave::String -> Kogame -> IO Kogame
commandSave filename game = do
    writeFile filename (show game)
    return game

commandSolve::Kogame -> IO Kogame 
commandSolve game = do
    let sol = solution game
    putStrLn (printSolution sol)
    return game

commandHint::Kogame -> IO Kogame 
commandHint game = do
    let hin = hint game
    putStrLn (printHint hin)
    return game

commandCheck::Kogame -> IO Kogame
commandCheck game = do
    if null (basicSolution game) then
        putStrLn "Map is not solvable"
    else
        putStrLn "Map is solvable"
    return game

commandPlay::Move -> Kogame -> IO Kogame
commandPlay func game = do
    d <- getPlayInput []
    let moves = map (\case
                    "Function" -> func
                    other -> fromString other) d
    if null moves then do
        putStrLn "You must input some directions to perform test!"
        return game
    else do
        putStrLn "test:"
        putStrLn ""
        print game
        runGo game moves

commandLoad::String -> IO Kogame 
commandLoad filename = do
    d <- readFile filename
    let board = (fromList . map (concat . words) . lines) d

    --
    -- "* * * * * - - - - - - - - - - - - - - - - - - - * * * * *" = "aa"
    -- "* * * * * b - - - - - - - - - - - - - - - - - b * * * * *" = "bb"
    -- ["aa","bb"]
    --let board = (fromList ((map (concat . words)) lines)) d -- map
    --sumEuler = sum . (map euler) . mkList
    --sumEuler = \x -> sum ((map euler) (mkList x))
    -- Input: words "aa bb cc \t dd \n ee"
    -- Output: ["aa","bb","cc","dd","ee"]
    --Input: concat [[1,2,3], [1,2,3]]
    --Output: [1,2,3,1,2,3]
    --fromList [] == empty
    --fromList [(5,"a"), (3,"b"), (5, "c")] == fromList [(5,"c"), (3,"b")]
    if not (isBoardValid board) then do
        return invalidGame
    else do
        let start = getRepresentationCoordinate board isStart --find @ coordinates
        let newBoard = setBoardRepresentation board start RollPath --
        return $ newKogame newBoard start

stripGetLine::IO String
stripGetLine = do 
    hFlush stdout
    stripInput <$> getLine

kogame::Kogame -> String -> IO()
kogame game file = do
    if checkWin game then -- checking if it is win?
        putStrLn "Congratulations! You win the game!"
    else do
        putStr "> " -- tell the user u can input sth 
        hFlush stdout
        line <- getLine --get user input
        let commands = words line 
        -- Input: words "aa bb cc \t dd \n ee"
        -- Output: ["aa","bb","cc","dd","ee"]
        -- ["load", "map1-2.txt"]
        let ps = tail commands -- inputs
        --["map1-2.txt"]
        -- Input: tail [1,2,3]
        -- Output: [2,3]
        if head commands == "load" then do -- load map1-2.txt
            newKogame <- commandLoad (head ps) --import sth to newKogame variable
            if newKogame == invalidGame then do
                putStrLn $ "Fail to read map from file: " ++ head ps
                kogame game file
            else do
                putStrLn "Read map successfully!"
                putStrLn "Initial:"
                print newKogame
                kogame newKogame (head ps)
        else do
            if game == invalidGame && head commands /= "generate" then do
                putStrLn "You needs to load a map before running other commands!"
                kogame game file
            else do
                case head commands of
                    "play" -> do
                        newKogame <- commandPlay (toFunction ps) game
                        kogame newKogame file
                    "check" -> do
                        newKogame <- commandCheck game
                        kogame newKogame file
                    "solve" -> do
                        newKogame <- commandSolve game
                        kogame newKogame file
                    "hint" -> do
                        newKogame <- commandHint game
                        kogame newKogame file
                    "save" -> do
                        newKogame <- commandSave (head ps) game
                        kogame newKogame file
                    "quit" -> do
                        return ()
                    "reset" -> do
                        newKogame <- commandReset game file
                        kogame newKogame file
                    "cmd" -> do
                        print command
                        kogame game file
                    "generate" -> do
                        newKogame <- commandGenerate ps
                        putStrLn "Generate map successfully!"
                        putStrLn "Your current map: "
                        print newKogame
                        kogame newKogame file
                    _ -> do
                        putStrLn $ "Invalid command: " ++ head commands
                        putStrLn "Use cmd to see all valid commands"
                        kogame game file

main::IO ()
main = kogame invalidGame ""

command::[String]
command = ["load", "play", "save", "solve", "check", "hint", "quit", "cmd", "reset", "generate"]

getPlayInput::[String] -> IO [String]
getPlayInput [] = do
    putStr "First direction: "
    m <- stripGetLine
    if m == "" then
        return []
    else
        getPlayInput [m]
getPlayInput ms = do
    putStr "Next direction: "
    m <- stripGetLine
    if m == "" then do
        return ms
    else
        getPlayInput $ ms ++ [m]


runGo::Kogame -> [Move] -> IO Kogame
runGo game [] = return game
runGo game (c:cs) = do
    let newGame = foldl startGo game (toMoveList c)
    if newGame == game then 
        putStrLn $ "Sorry, error: cannot move to the " ++
                    show c ++ ".\n" ++ "Your current board:"
    else
        putStrLn ""
    print newGame
    let currentBonus = bonusNum (getBoard newGame)
    if currentBonus /= bonusNum (getBoard game) then
        case currentBonus of
            2 -> putStrLn "got the first bonus!"
            1 -> putStrLn "got the second bonus!"
            0 -> putStrLn "got the last bonus!"
            _ -> putStrLn "error number of bonus!"
    else
        putStr ""
    runGo newGame cs