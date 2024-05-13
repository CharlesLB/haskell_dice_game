module Core.Dice (Dice, initializeDices) where

import Auxiliaries.Random (randomInt, randomInts)

type Dice = Int

--COMO DEVERIA SER A INICIALIZAÇÃO CASO A IMPORTAÇÃO FUNCIONASSE:
-- initializeDices :: Int -> IO Dices
-- initializeDices numDice = replicateM numDice (randomRIO (1, 6))
initializeDices :: Int -> IO [Dice]
initializeDices numDice = do
    let randomDiceValues = take numDice (randomInts 42)
    return randomDiceValues