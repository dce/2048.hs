import System.Random

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

emptyTiles :: [[Tile]] -> Int -> Int -> [(Int, Int)]
emptyTiles [] _ _  = []
emptyTiles ([] : rs) r c = emptyTiles rs (r + 1) 0
emptyTiles ((Empty : ts) : rs) r c = (r, c) : emptyTiles (ts : rs) r (c + 1)
emptyTiles ((_ : ts) : rs) r c = emptyTiles (ts : rs) r (c + 1)

setTile :: [[Tile]] -> Int -> Int -> Int -> [[Tile]]
setTile [] _ _ _ = []
setTile ((t : ts) : rs) 0 0 v = ((Val v) : ts) : rs
setTile ((t : ts) : rs) 0 c v = let (r : _) = setTile [ts] 0 (c - 1) v in
                                (t : r) : rs
setTile (ts : rs) r c v = ts : setTile rs (r - 1) c v

getNextMove :: [[Tile]] -> Move
getNextMove board = if canMove R board then R
                    else if canMove D board then D
                    else if canMove L board then L
                    else U

addRandomTile :: [[Tile]] -> IO [[Tile]]
addRandomTile board = do
  let empties = emptyTiles board 0 0
  idx <- randomRIO (0, (length empties - 1))
  let (r, c) = empties !! idx
  rv <- randomRIO (1 :: Int, 10)
  let v = if rv == 10 then 4 else 2
  return (setTile board r c v)

iter :: [[Tile]] -> IO ()
iter board = do
  putStrLn (show board)
  newState <- addRandomTile (applyMove (getNextMove board) board)

  if victory newState then putStrLn ("Victory! " ++ show newState)
  else if gameOver newState then putStrLn ("Failure! " ++ show newState)
  else iter newState

main :: IO ()
main = do
  let board = [ [Empty, Empty, Empty, Empty],
                [Empty, Empty, Empty, Empty],
                [Empty, Empty, Empty, Empty],
                [Empty, Empty, Empty, Empty] ]

  board <- addRandomTile board
  board <- addRandomTile board

  iter board
