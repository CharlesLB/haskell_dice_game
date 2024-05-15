module Core.Game (newGameState, playGame, initializingGame) where

import Core.Dice (Dice(..))
import Core.Players.Player (Player(..), PlayerType(..))
import Core.Players.HumanPlayer (HumanPlayer(..))
import Core.Players.BotPlayer (BotPlayer(..), BotLevel(..))
import Core.UI (initializingDices, initializingHumanPlayer, initializingBotPlayer, getPlayerMove)
import Lib.Printer (printStateCurrent, printChosenMove)

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

-- type GameMonad a = StateT GameState IO a

playGame :: GameState -> IO ()
playGame gameState = do
    if (nextPlayer gameState) == Human
                                then do
                                      printStateCurrent (playerName (humanPlayer gameState)) (dices gameState)
                                      (choice, index, action) <- getPlayerMove (dices gameState)
                                      case choice of
                                          1 -> do
                                              let chosenDice = dices gameState !! (index - 1)
                                              printChosenMove (humanName (humanPlayer gameState)) chosenDice index action
                                          2 -> putStrLn $ "Jogador escolheu retirar o dado " ++ show index
                                else printStateCurrent (playerName (botPlayer gameState)) (dices gameState)

initializingGame :: IO ()
initializingGame = do
  dices <- initializingDices
  human <- initializingHumanPlayer
  bot <- initializingBotPlayer

  let initialState = newGameState human bot dices Human
  playGame initialState

  return ()  -- Conclui a ação IO ()


