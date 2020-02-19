module Game where

data Tile = Val Int | Empty deriving (Show, Eq)

type Row = [Tile]

type Board = [Row]

data Move = L | R | U | D deriving (Show, Eq)

nthCol :: Board -> Int -> Row
nthCol board col = map (!! col) board

rot :: Board -> Board
rot board = map (reverse . nthCol board) [0..3]

slideRowLeft :: Row -> Row
slideRowLeft row = filter (/= Empty) row ++ filter (== Empty) row

combineMatches :: Row -> Row
combineMatches [] = []
combineMatches (Val x : Val y : ts) = if x == y
                                        then Val (x + y) : combineMatches (ts ++ [Empty])
                                        else Val x : combineMatches (Val y : ts)
combineMatches (t : ts) = t : combineMatches ts

slideLeft :: Board -> Board
slideLeft = map (combineMatches . slideRowLeft)

applyMove :: Move -> Board -> Board
applyMove L = slideLeft
applyMove D = rot . slideLeft . rot . rot . rot
applyMove R = rot . rot . slideLeft . rot . rot
applyMove U = rot . rot . rot . slideLeft . rot

canMove :: Move -> Board -> Bool
canMove move board = applyMove move board /= board

possibleMoves :: Board -> [Move]
possibleMoves board = filter (\m -> canMove m board) [L, R, U, D]

gameOver :: Board -> Bool
gameOver board = possibleMoves board == []

victory :: Board -> Bool
victory [] = False
victory ([] : rs) = victory rs
victory ((Val 2048 : _) : _) = True
victory ((t : ts) : rs) = victory (ts : rs)

emptyTiles :: Board -> Int -> Int -> [(Int, Int)]
emptyTiles [] _ _  = []
emptyTiles ([] : rs) r c = emptyTiles rs (r + 1) 0
emptyTiles ((Empty : ts) : rs) r c = (r, c) : emptyTiles (ts : rs) r (c + 1)
emptyTiles ((_ : ts) : rs) r c = emptyTiles (ts : rs) r (c + 1)

setTile :: Board -> Int -> Int -> Int -> Board
setTile [] _ _ _ = []
setTile ((t : ts) : rs) 0 0 v = ((Val v) : ts) : rs
setTile ((t : ts) : rs) 0 c v = let (r : _) = setTile [ts] 0 (c - 1) v in
                                (t : r) : rs
setTile (ts : rs) r c v = ts : setTile rs (r - 1) c v
