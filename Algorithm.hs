module Algorithm where

import Game
import Data.List

maxMoveBy :: (Move -> Board -> Int) -> Board -> Move
maxMoveBy f board =
  let comparison = (\m1 m2 -> compare (f m1 board) (f m2 board)) in
  maximumBy comparison (possibleMoves board)

tileScore :: Tile -> Int -> Int -> Int
tileScore Empty _ _ = 0
tileScore (Val n) r c = (n ^ 3) * (r + 1) * (c + 1)

rowScore :: [Tile] -> Int -> Int
rowScore ts r = sum (map (\c -> tileScore (ts !! c) r c) [0..3])

score :: Board -> Int
score board = sum (map (\r -> rowScore (board !! r) r) [0..3])

moveScore :: Move -> Board -> Int
moveScore m board = score (applyMove m board)

bestNextScore :: Move -> Board -> Int
bestNextScore m board =
  let nextBoard = applyMove m board in
  let nextMove  = maxMoveBy moveScore nextBoard in
  score (applyMove nextMove nextBoard)

getNextMove :: Board -> Move
getNextMove board = maxMoveBy bestNextScore board
