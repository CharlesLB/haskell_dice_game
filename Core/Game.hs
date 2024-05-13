module Core.Game (Dices, gameStateBase, game, initializeDices, initializeHumanPlayer, initializeBotPlayer, initializeGame) where

import Core.Dice (Dice)
import Core.Players.HumanPlayer (HumanPlayer(..))
import Core.Players.BotPlayer (BotPlayer(..), BotLevel(..))
import Auxiliaries.Random (randomInt, randomInts)

-- import System.Random
import Control.Monad (replicateM)
import Control.Monad.State

type Dices = [Dice]
data GameState = GameState
  { humanPlayer :: HumanPlayer,
    botPlayer :: BotPlayer,
    dices :: Dices
  } deriving (Show)

gameStateBase :: GameState
gameStateBase = GameState
  { humanPlayer = HumanPlayer "Player1", botPlayer = BotPlayer "Bot1" Easy, dices = []
  }

type GameMonad a = StateT GameState IO a

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

initializeGame :: HumanPlayer -> BotPlayer -> Dices -> GameMonad ()
initializeGame human bot dices = do
  let initialState = GameState human bot dices
  put initialState

game :: IO ()
game = do
  putStrLn "Welcome to the Dice Game!"


