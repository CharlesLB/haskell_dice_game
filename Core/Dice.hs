module Core.Dice (Dice(..), initializeDices, possibleRotations) where

import Lib.Random (randomInts)

import Control.Monad.State

data Dice = Dice { value :: Int }
    deriving (Show)

randomDiceValues :: Int -> [Dice]
randomDiceValues seed = map (\val -> Dice val) (randomInts seed)

--COMO DEVERIA SER A INICIALIZAÇÃO CASO A IMPORTAÇÃO FUNCIONASSE:
-- initializeDices :: Int -> IO Dices
-- initializeDices numDice = replicateM numDice (randomRIO (1, 6))
-- TODO: mudar para o initializeDICE e colocar no Game
initializeDices :: Int -> IO [Dice]
initializeDices numDice = do
    let randomDiceList = randomDiceValues 42
        selectedDiceList = take numDice randomDiceList
    return selectedDiceList

possibleRotations :: Dice -> [Int]
possibleRotations (Dice currentValue) =
    filter (\x -> x < currentValue && x + currentValue /= 7) [1..6]