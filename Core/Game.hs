module Core.Game (newGameState, playGame, initializingGame) where

import Core.Dice (Dice(..))
import Core.Players.Player (Player(..), PlayerType(..))
import Core.Players.HumanPlayer (HumanPlayer(..))
import Core.Players.BotPlayer (BotPlayer(..), BotLevel(..))
import Core.UI (initializingDices, initializingHumanPlayer, initializingBotPlayer, getPlayerMove)
import Lib.Printer (printStateCurrent, printChosenMove, printDiceConfiguration)

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

updateNextPlayerGameState :: GameState -> PlayerType-> GameState
updateNextPlayerGameState gameState playerType = gameState { nextPlayer = playerType }

isGameOver :: GameState -> Bool
isGameOver gameState = null (dices gameState)

-- type GameMonad a = StateT GameState IO a

updateDiceAtIndex :: Int -> Int -> [Dice] -> [Dice]
updateDiceAtIndex _ _ [] = []  -- Caso base: lista vazia
updateDiceAtIndex index newValue (dice:dices)
    | index < 1 = dice:dices  -- Se o índice for menor que 1, retornar a lista original
    | index == 1 = (Dice newValue):dices  -- Atualizar o dado no índice 1
    | otherwise = dice : updateDiceAtIndex (index - 1) newValue dices

removeDiceAtIndex :: Int -> [Dice] -> [Dice]
removeDiceAtIndex _ [] = []  -- Se a lista estiver vazia, retorna uma lista vazia
removeDiceAtIndex index (dice:dices)
    | index < 1 = dice:dices  -- Se o índice for menor que 1, retorna a lista original
    | index == 1 = dices  -- Se o índice for 1, remove o primeiro dado da lista
    | otherwise = dice : removeDiceAtIndex (index - 1) dices

playGame :: GameState -> IO ()
playGame gameState = do
    if (nextPlayer gameState) == Human
                                then do
                                      printStateCurrent (playerName (humanPlayer gameState)) (dices gameState)
                                      (choice, index, value) <- getPlayerMove (dices gameState)
                                      case choice of
                                          1 -> do
                                              let chosenDice = dices gameState !! (index - 1)
                                              printChosenMove (humanName (humanPlayer gameState)) chosenDice index value

                                              let updatedDiceList = updateDiceAtIndex index value (dices gameState)

                                              let actualizedState = newGameState (humanPlayer gameState) (botPlayer gameState) updatedDiceList Bot
                                              
                                              printStateCurrent (playerName (botPlayer actualizedState)) (dices actualizedState)
                                              
                                              playGame actualizedState
                                          2 -> do
                                              putStrLn $ "Jogador escolheu retirar o dado " ++ show index
                                              let updatedDiceList = removeDiceAtIndex index (dices gameState)
                                              let actualizedState = newGameState (humanPlayer gameState) (botPlayer gameState) updatedDiceList Bot
                                              
                                              printStateCurrent (playerName (botPlayer actualizedState)) (dices actualizedState)
                                              
                                              playGame actualizedState
                                else print("Vez do bot")

initializingGame :: IO ()
initializingGame = do
  dices <- initializingDices
  human <- initializingHumanPlayer
  bot <- initializingBotPlayer

  let initialState = newGameState human bot dices Human
  playGame initialState

  return ()  -- Conclui a ação IO ()


