{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}
module Core.Board.Board (Board (..), initializeBoard, getPossibleDicesToRotate, getPossibleDicesToRemove, updateDiceByIndex, removeDiceByIndex, isGameOver) where

import Control.Monad (replicateM)
import Core.Board.Dice (Dice (..), initializeDice, possibleRotations)
import Types.Move (Index)

type Board = [Dice]

initializeBoard :: Int -> IO Board
initializeBoard numDice = replicateM numDice initializeDice

getPossibleDicesToRemove :: Board -> [(Int, Int)]
getPossibleDicesToRemove board =
  [(index, value dice) | (index, dice) <- zip [0 ..] board, value dice == 1]

getPossibleDicesToRotate :: Board -> [(Int, Int)]
getPossibleDicesToRotate board =
  [(index, value dice) | (index, dice) <- zip [0 ..] board, value dice /= 1]

updateDiceByIndex :: Board -> Index -> Int -> Board
updateDiceByIndex [] _ _ = []
updateDiceByIndex (dice : board) index newValue
  | index < 0 = dice : board
  | index == 0 = Dice newValue : board
  | otherwise = dice : updateDiceByIndex board (index - 1) newValue

removeDiceByIndex :: Board -> Index -> Board
removeDiceByIndex [] _ = []
removeDiceByIndex (dice : board) index
  | index < 0 = dice : board
  | index == 0 = board
  | otherwise = dice : removeDiceByIndex board (index - 1)

isGameOver :: Board -> Bool
isGameOver board = null board
