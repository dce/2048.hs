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

rowScore :: [Tile] -> Int -> Int
rowScore ts r = sum (map tilePairScore (tilePairs ts)) * (r + 1)

score :: [[Tile]] -> Int
score board = sum (map (\r -> rowScore (board !! r) r) [0..3])

moveScore :: Move -> [[Tile]] -> Int
moveScore m board = score (applyMove m board)

bestNextScore :: Move -> [[Tile]] -> Int
bestNextScore m board =
  let nextBoard = applyMove m board in
  let nextMove  = maxMoveBy moveScore nextBoard in
  score (applyMove nextMove nextBoard)

worstRandomTile :: Move -> [[Tile]] -> Int
worstRandomTile m board =
  let nextBoard = applyMove m board in
  let empties = emptyTiles nextBoard 0 0 in
  let s = \(r, c) v -> score (setTile nextBoard r c v) in
  let (r, c) = maximumBy (\t1 t2 -> compare (s t1 2) (s t2 2)) empties in
  score (setTile nextBoard r c 2)

getNextMove :: [[Tile]] -> Move
getNextMove board = maxMoveBy moveScore board
