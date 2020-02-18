module Algorithm where

import Game
import Data.List

maxMoveBy :: (Move -> Board -> Int) -> Board -> Move
maxMoveBy f board =
  let comparison = (\m1 m2 -> compare (f m1 board) (f m2 board)) in
  maximumBy comparison (possibleMoves board)

tileScore :: Tile -> Int
tileScore Empty = 0
tileScore (Val n) = n

tilePairs :: Row -> [(Tile, Tile)]
tilePairs [] = []
tilePairs [t] = []
tilePairs (t1 : t2 : ts) = (t1, t2) : tilePairs (t2 : ts)

tilePairScore :: (Tile, Tile) -> Int
tilePairScore (t1, t2) =
  let diff = tileScore t2 - tileScore t1 in
  if diff < 0 then diff * 5 else diff

rowScore :: Row -> Int -> Int
rowScore ts r = sum (map tilePairScore (tilePairs ts)) * (r + 1)

score :: Board -> Int
score board =
  let rowScores = map (\r -> rowScore (board !! r) r) [0..3] in
  sum rowScores

moveScore :: Move -> Board -> Int
moveScore m board = score (applyMove m board)

getNextMove :: Board -> Move
getNextMove board = maxMoveBy moveScore board
