module Algorithm where

import Game
import Data.List

maxMoveBy :: (Move -> [[Tile]] -> Int) -> [[Tile]] -> Move
maxMoveBy f board =
  let comparison = (\m1 m2 -> compare (f m1 board) (f m2 board)) in
  maximumBy comparison (possibleMoves board)

tileScore :: Tile -> Int
tileScore Empty = 0
tileScore (Val n) = n

tilePairs :: [Tile] -> [(Tile, Tile)]
tilePairs [] = []
tilePairs [t] = []
tilePairs (t1 : t2 : ts) = (t1, t2) : tilePairs (t2 : ts)

tilePairScore :: (Tile, Tile) -> Int
-- tilePairScore (t1, t2) =
--   let diff = tileScore t2 - tileScore t1 in
--   if diff < 0 then diff * 5 else diff
tilePairScore (Empty, Empty) = 0
tilePairScore (Empty, Val n) = n
tilePairScore (Val n, Empty) = n * (-1)
tilePairScore (Val n, Val m) = let diff = m - n in
                               if diff < 0 then diff * 5 else diff

rowScore :: [Tile] -> Int
-- rowScore ts r = sum (map tilePairScore (tilePairs ts)) * (r + 1)
rowScore ts = sum (map (\t -> (tileScore t) ^ 3) ts)

score :: [[Tile]] -> Int
score board = sum (map rowScore board)

moveScore :: Move -> [[Tile]] -> Int
moveScore m board = score (applyMove m board)

mirror :: [[Tile]] -> [[Tile]]
mirror board = map (nthCol board) [0..3]

moveScoreWithMirror:: Move -> [[Tile]] -> Int
moveScoreWithMirror m board =
  let newBoard = applyMove m board in
  let mirrored = mirror newBoard in
  score newBoard + score mirrored

bestNextScore :: Move -> [[Tile]] -> Int
bestNextScore m board =
  let nextBoard = applyMove m board in
  let nextMove  = maxMoveBy moveScore nextBoard in
  score (applyMove nextMove nextBoard)

vectorize :: [[Tile]] -> [Tile]
vectorize (r1 : r2 : r3 : r4 : []) = (reverse r1) ++ r2 ++ (reverse r3) ++ r4

isIncrease :: (Tile, Tile) -> Bool
isIncrease (Empty, Empty) = False
isIncrease (Empty, _) = True
isIncrease (_, Empty) = False
isIncrease (Val m, Val n) = n > m

tilesIncreased :: Move -> [[Tile]] -> [Int]
tilesIncreased m board =
  let vector = vectorize board in
  let nextVector = vectorize (applyMove m board) in
  let pairs = zip vector nextVector in
  filter (\ i -> isIncrease (pairs !! i)) [0..15]

maxTileIncreased :: Move -> [[Tile]] -> Int
maxTileIncreased m board = head (reverse (tilesIncreased m board))

getNextMove :: [[Tile]] -> Move
getNextMove board = maxMoveBy maxTileIncreased board
