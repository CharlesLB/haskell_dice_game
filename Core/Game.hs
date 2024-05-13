module Core.Game (newGameState, playGame) where

import Core.Dice (Dice)
import Core.Players.Player (Player(..), PlayerType(..))
import Core.Players.HumanPlayer (HumanPlayer(..))
import Core.Players.BotPlayer (BotPlayer(..), BotLevel(..))
import Lib.Printer (printStateCurrent)

-- import System.Random
import Control.Monad (replicateM)
import Control.Monad.State

data GameState = GameState
  { humanPlayer :: HumanPlayer, 
    botPlayer :: BotPlayer,
    dices :: [Dice], 
    nextPlayer :: PlayerType
  } deriving (Show)

newGameState :: HumanPlayer -> BotPlayer -> [Dice] -> PlayerType -> GameState
newGameState humanPlayer botPlayer dices nextPlayer = GameState
    { humanPlayer = humanPlayer
    , botPlayer = botPlayer
    , dices = dices 
    , nextPlayer = nextPlayer
    }

updateGameState :: GameState -> [Dice] -> GameState
updateGameState gameState newDices = gameState { dices = newDices }

isGameOver :: GameState -> Bool
isGameOver gameState = null (dices gameState)

type GameMonad a = StateT GameState IO a

playGame :: GameState -> IO ()
playGame gameState = do
    if (nextPlayer gameState) == Bot
                                then printStateCurrent (playerName (humanPlayer gameState)) (dices gameState)
                                else printStateCurrent (playerName (botPlayer gameState)) (dices gameState)


