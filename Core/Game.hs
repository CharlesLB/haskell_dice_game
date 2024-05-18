{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use foldM" #-}
module Core.Game (GameState (..), newGameState, playGame, initializingGame) where

import Control.Monad (replicateM)
import Core.Board.Board (Board (..), initializeBoard)
import Core.Board.Dice (Dice (..), initializeDice, possibleRotations)
import Core.Players.BotPlayer (BotPlayer (..), initializeBotPlayer)
import Core.Players.HumanPlayer (HumanPlayer (..), initializeHumanPlayer)
import Core.Players.Player (Player (..), PlayerType (..), play, playerLevel, playerName, playerType)
import Core.UI (getLevelBotPlayer, getNameHumanPlayer, getNumberOfDices, getPlayerMove)
-- import Data.Map (Map)
import Lib.Printer (printChosenMove, printDiceConfiguration, printStateCurrent)
import System.Random (randomRIO)
import Types.BotLevel (BotLevel (..))

data GameState = GameState
  { players :: [Player],
    board :: Board
  }
  deriving (Show)

newGameState :: Player -> Player -> Board -> GameState
newGameState player1 player2 board =
  GameState
    { players = [player1, player2],
      board = board
    }

isGameOver :: GameState -> Bool
isGameOver gameState = null (board gameState)

updateDiceByIndex :: Board -> Int -> Int -> Board
updateDiceByIndex [] _ _ = []
updateDiceByIndex (dice : board) index newValue
  | index < 0 = dice : board
  | index == 0 = Dice newValue : board
  | otherwise = dice : updateDiceByIndex board (index - 1) newValue

removeDiceByIndex :: Board -> Int -> Board
removeDiceByIndex [] _ = []
removeDiceByIndex (dice : board) index
  | index < 0 = dice : board
  | index == 0 = board
  | otherwise = dice : removeDiceByIndex board (index - 1)

easyBotMove :: Board -> IO (Int, Int, Int) -- (choice, index, value)
easyBotMove board = do
  let numDices = length board
  randomIndex <- (randomRIO :: (Int, Int) -> IO Int) (0, numDices - 1)
  let chosenDice = board !! randomIndex
  if value chosenDice /= 1
    then do
      let rotations = possibleRotations chosenDice
      let n = length rotations
      indexDicesToRotation <- randomRIO (0, n - 1)
      let newValue = rotations !! indexDicesToRotation
      return (1, randomIndex, newValue)
    else do
      return (2, randomIndex, 0)

-- playRound :: GameState -> IO ()
-- playRound gameState
--   | nextPlayer gameState == Human = do
--       (choice, index, value) <- getPlayerMove (board gameState)

--       let actualizedState = case choice of
--             1 ->
--               let updatedboard = updateDiceByIndex (board gameState) (index - 1) value
--                in gameState {board = updatedboard}
--             2 ->
--               let updatedboard = removeDiceByIndex (board gameState) (index - 1)
--                in gameState {board = updatedboard}
--       let chosenDice = board gameState !! (index - 1)
--       printChosenMove choice (playerName (humanPlayer actualizedState)) chosenDice index value
--       printStateCurrent (playerName (humanPlayer actualizedState)) (board actualizedState)
--       if isGameOver actualizedState
--         then putStrLn "Humano venceu"
--         else playRound actualizedState
--   | otherwise = do
--       (choice, index, value) <- easyBotMove (board gameState)

--       let actualizedState = case choice of
--             1 ->
--               let updatedboard = updateDiceByIndex (board gameState) index value
--                in gameState {board = updatedboard}
--             2 ->
--               let updatedboard = removeDiceByIndex (board gameState) index
--                in gameState {board = updatedboard}

--       let chosenDice = board gameState !! index
--       printChosenMove choice (playerName (botPlayer actualizedState)) chosenDice (index + 1) value
--       printStateCurrent (playerName (botPlayer actualizedState)) (board actualizedState)

--       if isGameOver actualizedState
--         then putStrLn "Bot venceu"
--         else playRound actualizedState

playGame :: GameState -> IO ()
playGame gameState = do
  newGameState <- playRound (players gameState) gameState
  playGame newGameState

playRound :: [Player] -> GameState -> IO GameState
playRound [] gameState = return gameState
playRound (p : ps) gameState = do
  newGameState <- playMove p gameState
  playRound ps newGameState

playMove :: Player -> GameState -> IO GameState
playMove player gameState = do
  play player gameState
  return gameState

initializingGame :: IO ()
initializingGame = do
  numDices <- getNumberOfDices
  board <- initializeBoard numDices
  printDiceConfiguration board

  nameHumanPlayer <- getNameHumanPlayer
  human <- initializeHumanPlayer nameHumanPlayer
  let playerHuman = HumanPlayerType human
  putStrLn $ "O nome do jogador do tipo " ++ show (playerType playerHuman) ++ " é: " ++ playerName playerHuman

  levelBotPlayer <- getLevelBotPlayer
  let nameBot = "Bot" ++ show levelBotPlayer

  bot <- initializeBotPlayer nameBot levelBotPlayer
  let playerBot = BotPlayerType bot
  putStrLn $ "O nome do jogador do tipo " ++ show (playerType playerBot) ++ " é: " ++ playerName playerBot ++ ". Ele é do nivel " ++ show (playerLevel playerBot)

  let initialState = case botLevel bot of
        Easy -> newGameState playerHuman playerBot board
        Hard -> newGameState playerBot playerHuman board

  printStateCurrent (playerName playerHuman) board
  playGame initialState

  return ()
