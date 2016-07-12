import Game
import Algorithm
import System.Random

addRandomTile :: Board -> IO Board
addRandomTile board = do
  let empties = emptyTiles board 0 0
  idx <- randomRIO (0, (length empties - 1))
  let (r, c) = empties !! idx
  rv <- randomRIO (1 :: Int, 10)
  let v = if rv == 10 then 4 else 2
  return (setTile board r c v)

iter :: Board -> IO ()
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
