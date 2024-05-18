{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}
module Core.Board.Board (Board (..), initializeBoard, possibleDicesToRotations, possibleDicesToRemovals, updateDiceByIndex, removeDiceByIndex, isGameOver) where

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

--  Criar um Tipo Move que é Int -> Int
updateDiceByIndex :: Board -> Int -> Int -> Board
updateDiceByIndex [] _ _ = []
updateDiceByIndex (dice : board) index newValue
  | index < 0 = dice : board
  | index == 0 = Dice newValue : board
  | otherwise = dice : updateDiceByIndex board (index - 1) newValue

-- Criar um Tipo Index que é Int
removeDiceByIndex :: Board -> Int -> Board
removeDiceByIndex [] _ = []
removeDiceByIndex (dice : board) index
  | index < 0 = dice : board
  | index == 0 = board
  | otherwise = dice : removeDiceByIndex board (index - 1)

isGameOver :: Board -> Bool
isGameOver board = null board
