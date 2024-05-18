module Core.Dice (Dice (..), initializeDice, possibleRotations) where

import Control.Monad.State
import System.Random

data Dice = Dice {value :: Int}
  deriving (Show)

initializeDice :: IO Dice
initializeDice = do
  diceValue <- randomRIO (1, 6)
  return (Dice diceValue)

possibleRotations :: Dice -> [Int]
possibleRotations (Dice currentValue) =
  filter (\x -> x < currentValue && x + currentValue /= 7) [1 .. 6]