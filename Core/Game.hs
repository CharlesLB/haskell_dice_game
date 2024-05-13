module Core.Game (game, initializeGame) where

import Core.Dice (Dice)
import Auxiliaries.Random (randomInt, randomInts)

-- import System.Random
import Control.Monad (replicateM)

type Dices = [Dice]

--COMO DEVERIA SER A INICIALIZAÇÃO CASO A IMPORTAÇÃO FUNCIONASSE:
-- initializeGame :: Int -> IO Dices
-- initializeGame numDice = replicateM numDice (randomRIO (1, 6))
initializeGame :: Int -> IO Dices
initializeGame numDice = do
    let randomDiceValues = take numDice (randomInts 42)
    return randomDiceValues

game :: IO ()
game = do
  putStrLn "Welcome to the Dice Game!"


