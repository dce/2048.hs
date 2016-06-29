data Tile = Val Int | Empty deriving (Show, Eq)

data Move = L | R | U | D

nthCol :: [[Tile]] -> Int -> [Tile]
nthCol board col = map (!! col) board

rot :: [[Tile]] -> [[Tile]]
rot board = map (reverse . nthCol board) [0..3]

insertAfterEmpties :: Tile -> [Tile] -> [Tile]
insertAfterEmpties t (Empty : ts) = Empty : insertAfterEmpties t ts
insertAfterEmpties t ts = t : ts

slideAcrossEmpties :: [Tile] -> [Tile]
slideAcrossEmpties [] = []
slideAcrossEmpties (t : ts) = insertAfterEmpties t (slideAcrossEmpties ts)

combineMatches :: [Tile] -> [Tile]
combineMatches [] = []
combineMatches (Val x : Val y : ts) = if x == y
                                        then Val (x + y) : combineMatches (ts ++ [Empty])
                                        else Val x : combineMatches (Val y : ts)
combineMatches (t : ts) = t : combineMatches ts

slideRight :: [[Tile]] -> [[Tile]]
slideRight = map (reverse . combineMatches . reverse . slideAcrossEmpties)

applyMove :: Move -> [[Tile]] -> [[Tile]]
applyMove R = slideRight
applyMove D = rot . slideRight . rot . rot . rot
applyMove L = rot . rot . slideRight . rot . rot
applyMove U = rot . rot . rot . slideRight . rot

canMove :: Move -> [[Tile]] -> Bool
canMove move board = applyMove move board /= board

gameOver :: [[Tile]] -> Bool
gameOver board = not (canMove L board ||
                      canMove R board ||
                      canMove U board ||
                      canMove D board)

victory :: [[Tile]] -> Bool
victory [] = False
victory ([] : rs) = victory rs
victory ((Val 2048 : _) : _) = True
victory ((t : ts) : rs) = victory (ts : rs)

addRandomTile :: [[Tile]] -> [[Tile]]
addRandomTile = undefined

board :: [[Tile]]
board = [ [Empty, Empty, Empty, Empty],
          [Empty, Empty, Empty, Empty],
          [Val 2, Empty, Empty, Val 2],
          [Empty, Empty, Empty, Empty] ]

