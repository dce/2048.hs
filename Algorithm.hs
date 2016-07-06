module Algorithm where

import Game
import Data.List

maxMoveBy :: (Move -> [[Tile]] -> Int) -> [[Tile]] -> Move
maxMoveBy f board =
  let comparison = (\m1 m2 -> compare (f m1 board) (f m2 board)) in
  maximumBy comparison (possibleMoves board)

tileToInt :: Tile -> Int
tileToInt Empty = 0
tileToInt (Val n) = n

horizontalNeighbors :: [[Tile]] -> Int -> Int -> [Tile]
horizontalNeighbors board r 0 = [ board !! r !! 1 ]
horizontalNeighbors board r 3 = [ board !! r !! 2 ]
horizontalNeighbors board r c = [ board !! r !! (c - 1) , board !! r !! (c + 1) ]

verticalNeighbors :: [[Tile]] -> Int -> Int -> [Tile]
verticalNeighbors board 0 c = [ board !! 1 !! c ]
verticalNeighbors board 3 c = [ board !! 2 !! c ]
verticalNeighbors board r c = [ board !! (r - 1) !! c , board !! (r + 1) !! c ]

neighbors :: [[Tile]] -> Int -> Int -> [Tile]
neighbors board r c = (horizontalNeighbors board r c) ++ (verticalNeighbors board r c)

logBase2 :: Int -> Int
logBase2 2 = 0
logBase2 n = 1 + logBase2 (n `div` 2)

adjustedScore :: Tile -> Tile -> Int
adjustedScore Empty _ = 0
adjustedScore _ Empty = 0
adjustedScore t1 t2 =
  let v1 = fromIntegral (tileToInt t1) in
  let v2 = fromIntegral (tileToInt t2) in
  let distance = abs ((logBase2 v1) - (logBase2 v2)) + 1 in
  v2 `div` distance

tileScore :: [[Tile]] -> Int -> Int -> Int
tileScore board r c =
  let tile = board !! r !! c in
  let base = tileToInt tile ^ 3 in
  let ns = neighbors board r c in
  base + sum (map (\n -> adjustedScore tile n) ns)

score :: [[Tile]] -> Int
score board =
  let tiles = [ (x, y) | x <- [0..3], y <- [0..3] ] in
  sum (map (\ (r, c) -> tileScore board r c) tiles)

moveScore :: Move -> [[Tile]] -> Int
moveScore m board = score (applyMove m board)

bestNextScore :: Move -> [[Tile]] -> Int
bestNextScore m board =
  let nextBoard = applyMove m board in
  let nextMove  = maxMoveBy moveScore nextBoard in
  score (applyMove nextMove nextBoard)

getNextMove :: [[Tile]] -> Move
getNextMove board = maxMoveBy bestNextScore board
