module Core.Game (game, initializeDices, initializeHumanPlayer, initializeBotPlayer) where

import Core.Dice (Dice)
import Core.Players.HumanPlayer (HumanPlayer(..))
import Core.Players.BotPlayer (BotPlayer(..), BotLevel(..))
import Auxiliaries.Random (randomInt, randomInts)

-- import System.Random
import Control.Monad (replicateM)

type Dices = [Dice]

--COMO DEVERIA SER A INICIALIZAÇÃO CASO A IMPORTAÇÃO FUNCIONASSE:
-- initializeDices :: Int -> IO Dices
-- initializeDices numDice = replicateM numDice (randomRIO (1, 6))
initializeDices :: Int -> IO Dices
initializeDices numDice = do
    let randomDiceValues = take numDice (randomInts 42)
    return randomDiceValues

initializeHumanPlayer :: String -> IO HumanPlayer
initializeHumanPlayer nameHumanPlayer = do
  let human = HumanPlayer { humanName = nameHumanPlayer }
  return human

initializeBotPlayer :: String -> BotLevel -> IO BotPlayer
initializeBotPlayer nameBotPlayer levelBotPlayer = do
  let bot = BotPlayer { botName = nameBotPlayer, botLevel =  levelBotPlayer}
  return bot

game :: IO ()
game = do
  putStrLn "Welcome to the Dice Game!"


