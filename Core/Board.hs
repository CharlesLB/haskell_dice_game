module Core.Board (Board (..), initializeBoard, possibleDicesToRotations, possibleDicesToRemovals) where

import Core.Dice (Dice (..), initializeDice, possibleRotations)
import Control.Monad (replicateM)

type Board = [Dice]

initializeBoard :: Int -> IO Board
initializeBoard numDice = do
  board <- replicateM numDice initializeDice
  return board

possibleDicesToRemovals :: Board -> [(Int, Int)]
possibleDicesToRemovals board =
  [(index, value dice) | (index, dice) <- zip [1 ..] board, value dice == 1]

possibleDicesToRotations :: Board -> [(Int, Int)]
possibleDicesToRotations board =
  [(index, value dice) | (index, dice) <- zip [1 ..] board, value dice /= 1]