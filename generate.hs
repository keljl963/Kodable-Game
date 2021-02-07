
module Generate where

import Representations
import Solution
import System.Random

randomCoordinate::Board -> IO Coordinate
randomCoordinate board = do
    wid <- randomRIO (0, width board - 1)
    hei <- randomRIO (0, height board - 1)
    if isTemp (getBoardRepresentation board (Coordinate hei wid)) then
        return $ Coordinate hei wid
    else
        randomCoordinate board

randomCoordinate2::Board -> IO Coordinate
randomCoordinate2 board = do
    wid <- randomRIO (0, width board - 1)
    hei <- randomRIO (0, height board - 1)
    if isRollPath (getBoardRepresentation board (Coordinate hei wid)) then
        return $ Coordinate hei wid
    else
        randomCoordinate2 board

randomMove::Move -> IO Move
randomMove m = do
    index <- randomRIO (0, length allM - 1)
    if m == allM !! index then
        randomMove m
    else
        return $ allM !! index
    where
        ms = ["Left", "Right", "Down", "Up"]
        allM = concat (replicate 15 (map fromString ms)) ++
                map (Cond (fromChar 'y') . fromString) ms ++
                map (Cond (fromChar 'p') . fromString) ms ++
                map (Cond (fromChar 'o') . fromString) ms

allMoves::IO [Move]
allMoves = do
    num <- randomRIO (5 :: Integer, 20)
    ms <- _moves num (fromString "")
    if isCond (head ms) then
        allMoves
    else
        return ms
    where
        _moves 0 _ = return []
        _moves num m = do
            this <- randomMove m
            ms <- _moves (num - 1) this
            return $ this:ms

stopMove::Board -> Move -> Coordinate -> IO Coordinate
stopMove board mt c
    | toString m == "Left" = do
        if getY c == 0 then do
            return c
        else do
            num <- randomRIO (0, getY c - 1)
            return $ Coordinate (getX c) num
    | toString m == "Right" = do
        if getY c == width board - 1 then do
            return c
        else do
            num <- randomRIO (getY c + 1, width board - 1)
            return $ Coordinate (getX c) num
    | toString m == "Up" = do
        if getX c == 0 then do
            return c
        else do
            num <- randomRIO (0, getX c - 1)
            return $ Coordinate num (getY c)
    | toString m == "Down" = do
        if getX c == height board - 1 then do
            return c
        else do
            num <- randomRIO (getX c + 1, height board - 1)
            return $ Coordinate num (getY c)
    | otherwise = do return c
    where
        m = fromCond mt

fillBoard::Board -> Move -> Coordinate -> Coordinate -> (Board, Coordinate)
fillBoard board m c stopCoord
    | c == stopCoord = (board, stopCoord)
    | isObstacle (getBoardRepresentation board newC) = (newBoard, c)
    | otherwise = fillBoard newBoard m newC stopCoord
    where
        newBoard = setBoardRepresentation board c RollPath
        newC = move m c

randGrassOrRollPath::IO Representation
randGrassOrRollPath = do
    r <- randomRIO (0 :: Integer, 100)
    if r > 90 then
        return RollPath
    else
        return Grass

_fillBoard::[Char] -> IO [Char]
_fillBoard [] = return []
_fillBoard (c:cs) = do
    left <- _fillBoard cs
    if c == ' ' then do
        t <- randGrassOrRollPath
        return $ toChar t : left
    else
        return $ c : left

fillBoard2::Board -> IO Board
fillBoard2 board = do
    let wid = width board
    b <- _fillBoard $ (concat . toList) board
    let bo = toBoard wid b
    return $ fromList bo
    where
        toBoard _ [] = []
        toBoard wid b = take wid b : toBoard wid (drop wid b)


randomMoveBoard::Board -> Move -> Coordinate -> IO (Board, Coordinate)
randomMoveBoard b m c = do
    stopCoord <- stopMove b m c
    let board = setBoardRepresentation b c RollPath
    if stopCoord == c then
        return (board, c)
    else do
        let newBAndC = fillBoard board m (move m c) stopCoord
        let newBoard = fst newBAndC
        let newC = snd newBAndC
        if isCond m then do
            let newBoard2 = setBoardRepresentation newBoard c (toRepresentation m)
            return (newBoard2, newC)
        else do
            if isValid newBoard (move m newC) then do
                let newBoard2 = setBoardRepresentation newBoard
                        (move m newC) (toRepresentation m)
                return (newBoard2, newC)
            else
                return (newBoard, newC)

setBoardBonus::Board -> Int -> IO Board
setBoardBonus board 0 = return board
setBoardBonus board num = do
    c <- randomCoordinate2 board
    let newBoard = setBoardRepresentation board c Bonus
    setBoardBonus newBoard (num - 1)

_randomBoard::Board -> [Move] -> Coordinate -> IO (Board, Coordinate)
_randomBoard board [] c = return (board, c)
_randomBoard board (m:ms) c = do
    bAndc <- randomMoveBoard board m c
    let newB = fst bAndc
    let newC = snd bAndc
    _randomBoard newB ms newC

randomBoard::Int -> Int -> IO Board
randomBoard wid hei = do
    let board = emptyBoard wid hei
    ball <- randomCoordinate board
    ms <- allMoves
    b <- _randomBoard board ms ball
    let bo = fst b
    let target = snd b
    let newB = setBoardRepresentation bo ball Start
    let newB2 = setBoardRepresentation newB target Target
    b2 <- setBoardBonus newB2 3
    fillBoard2 b2
    

generate::Int -> Int -> IO Kogame
generate wid hei = do
    b <- randomBoard wid hei
    let start = getRepresentationCoordinate b isStart
    let newB = setBoardRepresentation b start RollPath
    let game = Kogame newB start
    if null (basicSolution game) then
        generate wid hei
    else
        return game
