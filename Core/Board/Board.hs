module Core.Board.Board (Board (..), initializeBoard, possibleDicesToRotations, possibleDicesToRemovals) where

import Control.Monad (replicateM)
import Core.Board.Dice (Dice (..), initializeDice, possibleRotations)

type Board = [Dice]

initializeBoard :: Int -> IO Board
initializeBoard numDice = replicateM numDice initializeDice

possibleDicesToRemovals :: Board -> [(Int, Int)]
possibleDicesToRemovals board =
  [(index, value dice) | (index, dice) <- zip [1 ..] board, value dice == 1]

possibleDicesToRotations :: Board -> [(Int, Int)]
possibleDicesToRotations board =
  [(index, value dice) | (index, dice) <- zip [1 ..] board, value dice /= 1]