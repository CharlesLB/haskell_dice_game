{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Core.Board.Dice (Dice (..), initializeDice, possibleRotations) where

import System.Random (randomRIO)

data Dice = Dice {value :: Int}
  deriving (Show)

initializeDice :: IO Dice
initializeDice = do
  diceValue <- randomRIO (1, 6)
  return (Dice diceValue)

possibleRotations :: Dice -> [Int]
possibleRotations (Dice currentValue) =
  filter (\x -> x < currentValue && x + currentValue /= 7) [1 .. 6]