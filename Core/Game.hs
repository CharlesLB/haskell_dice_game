module Core.Game (Dices, newGameState, game, initializeDices, initializeHumanPlayer, initializeBotPlayer) where

import Core.Dice (Dice)
import Core.Players.Player (Player(..), PlayerType)
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
    dices :: Dices, 
    nextPlayer :: PlayerType
  } deriving (Show)

newGameState :: HumanPlayer -> BotPlayer -> Dices -> PlayerType -> GameState
newGameState humanPlayer botPlayer dices nextPlayer = GameState
    { humanPlayer = humanPlayer
    , botPlayer = botPlayer
    , dices = dices 
    , nextPlayer = nextPlayer
    }

updateGameState :: GameState -> Dices -> GameState
updateGameState gameState newDices = gameState { dices = newDices }

isGameOver :: GameState -> Bool
isGameOver gameState = null (dices gameState)

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

game :: IO ()
game = do
  putStrLn "Welcome to the Dice Game!"


