{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use foldM" #-}
module Core.Game (game) where

import Control.Monad (replicateM)
import Core.Board.Board (Board (..), initializeBoard, isGameOver, removeDiceByIndex, updateDiceByIndex)
import Core.Board.Dice (Dice (..), initializeDice, possibleRotations)
import Core.Players.BotPlayer (BotPlayer (..), initializeBotPlayer)
import Core.Players.HumanPlayer (HumanPlayer (..), initializeHumanPlayer)
import Core.Players.Player (Player (..), PlayerType (..), play, playerLevel, playerName, playerType)
import Core.UI (getPlayerMove, getSetupData)
import Lib.Printer (printBoard, printChosenMove, printStateCurrent)
import Lib.Reader (readBotLevel)
import System.Random (randomRIO)
import Types.BotLevel (BotLevel (..))
import Types.Move (Move (..))
import Types.SetupData (SetupData (..))

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

playGame :: GameState -> IO ()
playGame gameState = do
  if isGameOver (board gameState)
    then do
      putStrLn "Jogo acabou"
      return ()
    else do
      newGameState <- playRound (players gameState) gameState
      playGame newGameState

playRound :: [Player] -> GameState -> IO GameState
playRound [] gameState = return gameState
playRound (p : ps) gameState = do
  if isGameOver (board gameState)
    then do
      return gameState
    else do
      newGameState <- playMove p gameState
      playRound ps newGameState

playMove :: Player -> GameState -> IO GameState
playMove player gameState = do
  move <- play player (board gameState)

  let actualizedState = case move of
        UpdateMove {updateIndex = index, newValue = val} ->
          let updatedBoard = updateDiceByIndex (board gameState) index (max val 0)
           in gameState {board = updatedBoard}
        RemoveMove {removeIndex = index} ->
          let updatedBoard = removeDiceByIndex (board gameState) index
           in gameState {board = updatedBoard}

  let index = case move of
        UpdateMove {updateIndex = index} -> index
        RemoveMove {removeIndex = index} -> index

  let chosenDice = board gameState !! index

  printChosenMove move (playerName player) chosenDice
  printStateCurrent (playerName player) (board actualizedState)

  if isGameOver (board actualizedState)
    then do
      putStrLn $ playerName player ++ " venceu"
      return actualizedState
    else return actualizedState

buildGame :: SetupData -> IO GameState
buildGame setupData = do
  board <- initializeBoard (numDices setupData)

  human <- initializeHumanPlayer (setupPlayerName setupData)
  let playerHuman = HumanPlayerType human
  putStrLn $ "O nome do jogador do tipo " ++ show (playerType playerHuman) ++ " é: " ++ playerName playerHuman

  bot <- initializeBotPlayer "Bot" (setupBotLevel setupData)
  let playerBot = BotPlayerType bot
  putStrLn $ "O nome do jogador do tipo " ++ show (playerType playerBot) ++ " é: " ++ playerName playerBot ++ ". Ele é do nivel " ++ show (playerLevel playerBot)

  let initialState = case botLevel bot of
        Easy -> newGameState playerHuman playerBot board
        Hard -> newGameState playerBot playerHuman board

  printStateCurrent (playerName playerHuman) board
  return initialState

game :: IO ()
game = do
  gameSetup <- getSetupData
  game <- buildGame gameSetup
  playGame game
