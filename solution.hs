
module Solution where

import Representations
import Data.List (transpose)

basicSolution::Kogame -> [[Move]]
basicSolution game = map getMoves vs
    where
        ss = moveMany [[game]]
        vs = filter validSolution ss
        validSolution gm = checkWin g && bonusNum (getBoard g) == 0
            where
                g = last gm

bestSolution::[[Move]] -> [Move]
bestSolution [] = []
bestSolution cs = foldl1 (\w t -> if length w > length t then t else w) cs

indexOf::[Move] -> [Move] -> Int
indexOf _ [] = 0
indexOf m ms
    | m == take (length m) ms = 0
    | otherwise = 1 + indexOf m (tail ms)

loopNum::[Move] -> Int
loopNum ms
    | length ms < 4 || take 2 ms `indexOf` drop 2 ms /= 0 = 1
    | otherwise = 1 + loopNum (drop 2 ms)

funcNum::Move -> [Move] -> Int
funcNum func ms
    | length ms < 3 || index == length ms = 0
    | otherwise = 1 + funcNum func (drop (index + 3) ms) 
    where
        index = toMoveList func `indexOf` ms

_loopSolution::[Move] -> [[Move]]
_loopSolution ms
    | lpNum > 1 = map (loops:) (_loopSolution (drop (lpNum * 2) ms)) ++ other
    | otherwise = other
    where
        lpNum = min (loopNum ms) 5
        loops = toLoop lpNum (map toString (take 2 ms))
        other = if length ms > 1 then [head ms : m | m <- _loopSolution (tail ms)] else [ms]

loopSolution::[Move] -> [Move]
loopSolution = bestSolution . _loopSolution

filterDuplicatedMove::[Move] -> [Move]
filterDuplicatedMove [] = []
filterDuplicatedMove (c:cs)
    | c `elem` cs = filterDuplicatedMove cs
    | otherwise = c:filterDuplicatedMove cs

getAllFuncs::[Move] -> [Move]
getAllFuncs ms = filterDuplicatedMove $
                    filter (\w -> funcNum w ms >= 2) $
                    map (toFunction . map toString) $
                    filter (\f -> length f == 3) $
                    transpose [ms, tail ms, (tail . tail) ms]

_funcSolution::Move -> [Move] -> [[Move]]
_funcSolution func ms
    | index == 0 = map (func:) (_funcSolution func (drop 3 ms)) ++ other
    | otherwise = other
    where
        index = toMoveList func `indexOf` ms
        other = if length ms > 1 then [head ms: m |
                    m <- _funcSolution func (tail ms)] else [ms]

hasFunc::[Move] -> Bool
hasFunc = any isFunc
cntFunc::[Move] -> Int
cntFunc = length . filter isFunc
getFunc::[Move] -> Move
getFunc = head . filter isFunc

splitFunc::[Move] -> [[Move]]
splitFunc ms
    | hasFunc ms = [take index ms] ++ [[getFunc ms]] ++ splitFunc (drop (index + 1) ms)
    | otherwise = [ms]
    where
        index = [getFunc ms] `indexOf` ms

optimise::[Move] -> [[Move]]
optimise ms = map (concatMap (\f -> if hasFunc f then f else loopSolution f) . splitFunc)
                (concatMap (`_funcSolution` ms) $ getAllFuncs ms) ++ [loopSolution ms]

solution::Kogame -> [Move]
solution = bestSolution . map (bestSolution . optimise) . basicSolution

printSolution::[Move] -> String
printSolution ms
    | null ms = "Cannot find a solution to map"
    | hasFunc ms =(unwords . map show) ms ++ " with " ++
                    (unwords . map show) (toMoveList (getFunc ms))
    | otherwise = (unwords . map show) ms

basicHint::Kogame -> [[Move]]
basicHint game = map getMoves $ filter (checkWin . last) $ moveMany [[game]]

hint::Kogame -> [Move]
hint = bestSolution . map loopSolution . basicHint

printHint::[Move] -> String
printHint ms
    | null ms = "Map is not solvable, No hint is avaiable"
    | otherwise = "Hint: next direction is " ++ show (head ms)
