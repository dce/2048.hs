module Algorithm where

import Game
import Data.List

tileScore :: Tile -> Int -> Int -> Int
tileScore Empty _ _ = 0
tileScore (Val n) r c = (n ^ 3) * (r + 1) * (c + 1)

rowScore :: [Tile] -> Int -> Int
rowScore ts r = sum (map (\c -> tileScore (ts !! c) r c) [0..3])

score :: [[Tile]] -> Int
score board = sum (map (\r -> rowScore (board !! r) r) [0..3])

moveScore :: Move -> [[Tile]] -> Int
moveScore m board = score (applyMove m board)

getNextMove :: [[Tile]] -> Move
getNextMove board =
  let comparison = \m1 m2 -> compare (moveScore m1 board) (moveScore m2 board) in
  maximumBy comparison (possibleMoves board)
