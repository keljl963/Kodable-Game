
module Representations where

import Data.List (intersperse)

data Representation =
    RollPath
    | Grass
    | ColorPath Char
    | Bonus
    | Start
    | Target
    | Temp
    deriving Eq

instance Show Representation where
    show r = [toChar r]

fromChar::Char -> Representation
fromChar '-' = RollPath
fromChar '*' = Grass
fromChar 'o' = ColorPath 'o'
fromChar 'p' = ColorPath 'p'
fromChar 'y' = ColorPath 'y'
fromChar 'b' = Bonus
fromChar '@' = Start
fromChar 't' = Target
fromChar _ = Temp

toChar::Representation -> Char
toChar RollPath = '-'
toChar Grass = '*'
toChar (ColorPath 'o') = 'o'
toChar (ColorPath 'p') = 'p'
toChar (ColorPath 'y') = 'y'
toChar Bonus = 'b'
toChar Start = '@'
toChar Target = 't'
toChar _ = ' '

isObstacle::Representation -> Bool
isObstacle Grass = True
isObstacle _ = False

isStart::Representation -> Bool
isStart Start = True
isStart _ = False 

isTarget::Representation -> Bool
isTarget Target = True
isTarget _ = False 

isColorPath::Representation -> Bool
isColorPath (ColorPath _) = True
isColorPath _ = False

isBonus::Representation -> Bool
isBonus Bonus = True
isBonus _ = False

isTemp::Representation -> Bool
isTemp Temp = True
isTemp _ = False

isRollPath::Representation -> Bool
isRollPath RollPath = True
isRollPath _ = False

data Coordinate = Coordinate Int Int
    deriving (Eq, Show)

getX::Coordinate -> Int
getX (Coordinate x _) = x
getY::Coordinate -> Int
getY (Coordinate _ y) = y

toLeft::Coordinate -> Coordinate
toLeft c = Coordinate (getX c) (getY c - 1)
toRight::Coordinate -> Coordinate
toRight c = Coordinate (getX c) (getY c + 1)
toDown::Coordinate -> Coordinate
toDown c = Coordinate (getX c + 1) (getY c)
toUp::Coordinate -> Coordinate
toUp c = Coordinate (getX c - 1) (getY c)

data Move = 
    Normal String
    | Loop Int Move Move
    | Function Move Move Move
    | Cond Representation Move
    deriving Eq

instance Show Move where
    show = toString

toString::Move -> String
toString (Normal str) = str
toString (Loop num m1 m2) = "Loop{" ++ show num ++ "}" ++ "{"
                            ++ show m1 ++ "," ++ show m2 ++ "}"
toString (Cond r m) = "Cond{" ++ show r ++ "}{" ++ show m ++ "}"
toString Function {} = "Function"

fromString::String -> Move
fromString str
    | "Cond" == take 4 str = Cond (fromChar $ str!!5) (fromString $ drop 8 (init str))
    | "Loop" == take 4 str = Loop (read [str!!5] :: Int) (fromString part1)
                                (fromString (drop (10 + length part1) (init str)))
    | otherwise = Normal str
    where
        part1 = takeWhile (/='}') (drop 8 str)

toRepresentation::Move -> Representation
toRepresentation (Cond r _) = r
toRepresentation _ = Grass

isFunc::Move -> Bool
isFunc Function {} = True
isFunc _ = False

toFunction::[String] -> Move
toFunction [m1, m2, m3] = Function (fromString m1) (fromString m2) (fromString m3)
toFunction _ = Normal ""

toLoop::Int -> [String] -> Move
toLoop num (m1:m2:_) = Loop num (fromString m1) (fromString m2)
toLoop _ _ = Normal ""

toMoveList::Move -> [Move]
toMoveList (Function m1 m2 m3) = [m1, m2, m3]
toMoveList (Loop num m1 m2) = concat $ replicate num [m1, m2]
toMoveList other = [other]

move::Move -> Coordinate -> Coordinate
move (Normal "Left") = toLeft
move (Normal "Right") = toRight
move (Normal "Down") = toDown
move (Normal "Up") = toUp
move (Cond _ m) = move m
move _ = id

isCond::Move -> Bool
isCond (Cond _ _) = True
isCond _ = False

fromCond::Move -> Move
fromCond (Cond _ m) = m
fromCond m = m

newtype Board = Board [[Representation]]
    deriving Eq

instance Show Board where
    show = unlines . map (intersperse ' ') . toList

fromList::[String] -> Board
fromList cs = Board (map (map fromChar) cs)

toList::Board -> [String]
toList (Board cs) = map (map toChar) cs

invalidBoard::Board
invalidBoard = Board []

width::Board -> Int
width = length . head . toList

height::Board -> Int
height = length . toList

getRepresentationCoordinate::Board -> (Representation -> Bool) -> Coordinate
getRepresentationCoordinate board func = uncurry Coordinate (head xy)
    where
        xy = [(i, j) | i <- [0..height board - 1],
                    j <- [0..width board - 1],
                    func $ getBoardRepresentation board (Coordinate i j)]

getBoardRepresentation::Board -> Coordinate -> Representation
getBoardRepresentation board c = fromChar $ (toList board !! getX c) !! getY c

setBoardRepresentation::Board -> Coordinate -> Representation -> Board
setBoardRepresentation board c r = fromList [[toChar $ if Coordinate i j /= c
                                    then getBoardRepresentation board (Coordinate i j)
                                    else r
                                            | j <- [0..width board - 1]]
                                            | i <- [0..height board - 1]]

bonusNum::Board -> Int
bonusNum board = length ["" | i <- [0..height board - 1],
                    j <- [0..width board - 1],
                isBonus $ getBoardRepresentation board (Coordinate i j)]

isBoardValid::Board -> Bool
isBoardValid = any (' ' `notElem`) . toList

isValid::Board -> Coordinate -> Bool
isValid board c = getX c >= 0 && getX c < height board &&
                    getY c >= 0 && getY c < width board &&
                    not (isObstacle (getBoardRepresentation board c))

data Kogame = Kogame Board Coordinate
    deriving Eq

getBoard::Kogame -> Board
getBoard (Kogame board _) = board

getStart::Kogame -> Coordinate
getStart (Kogame _ c) = c

invalidGame::Kogame
invalidGame = Kogame invalidBoard (Coordinate 0 0)

canMove::Kogame -> Move -> Bool
canMove kogame m = (not (isColorPath r) && not (isCond m))
                    || isColorPath r && isCond m && r == toRepresentation m
    where
        r = getBoardRepresentation (getBoard kogame) (getStart kogame)

instance Show Kogame where
    show kogame = show (setBoardRepresentation (getBoard kogame) (getStart kogame) Start)

checkWin::Kogame -> Bool
checkWin kogame
    | kogame == invalidGame = False
    | otherwise = getStart kogame == getRepresentationCoordinate (getBoard kogame) isTarget

startGo::Kogame -> Move -> Kogame
startGo kogame m
    | not (canMove kogame m) ||
        pos == getStart kogame ||
        not (isValid (getBoard kogame) pos) ||
        isTarget (getBoardRepresentation (getBoard kogame) (getStart kogame)) ||
        isObstacle (getBoardRepresentation (getBoard kogame) pos) = kogame
    | otherwise = startGo (Kogame newBoard pos) newMove
    where
        pos = move m $ getStart kogame
        newMove = fromCond m
        newBoard = if isBonus (getBoardRepresentation (getBoard kogame) pos) then
            setBoardRepresentation (getBoard kogame) pos RollPath else getBoard kogame

getValidMove::Kogame -> [Move]
getValidMove game
    | isColorPath m = map (Cond m) ms
    | otherwise = ms
    where
        m = getBoardRepresentation (getBoard game) (getStart game)
        ms = map fromString ["Left", "Right", "Down", "Up"]

moveOnce::[Kogame] -> ([[Kogame]], Bool)
moveOnce games
    | checkWin g = ([games], True)
    | null ms = ([], True)
    | otherwise  = (map (\m -> games ++ [startGo g m]) ms, False)
    where
        g = last games
        ms = filter (\m -> startGo g m `notElem` games) (getValidMove g)

moveMany::[[Kogame]] -> [[Kogame]]
moveMany games
    | isFinished = newGames
    | otherwise = moveMany newGames
    where
        gs = map moveOnce games
        isFinished = all snd gs
        newGames = concatMap fst gs

gameToMove::Kogame -> Kogame -> Move
gameToMove g1 g2
    | isColorPath r = Cond r d
    | otherwise  = d
    where
        r = getBoardRepresentation (getBoard g1) (getStart g1)
        d = coordinateToMove (getStart g1) (getStart g2)
        coordinateToMove  c1 c2
            | getY c1 > getY c2 = fromString "Left"
            | getY c1 < getY c2 = fromString "Right"
            | getX c1 < getX c2 = fromString "Down"
            | getX c1 > getX c2 = fromString "Up"
            | otherwise = fromString ""

getMoves::[Kogame] -> [Move]
getMoves games = zipWith gameToMove games (tail games)

stripInput::String -> String
stripInput = filter (/= ' ')

emptyBoard::Int -> Int -> Board
emptyBoard wid hei = fromList (replicate hei $ replicate wid ' ')
