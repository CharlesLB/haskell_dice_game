module Core.Game (game) where

import Core.Board.Board (Board (..), initializeBoard, isGameOver, removeDiceByIndex, updateDiceByIndex)
import Core.Players.BotPlayer (BotPlayer (..), initializeBotPlayer)
import Core.Players.HumanPlayer (HumanPlayer (..), initializeHumanPlayer)
import Core.Players.Player (Player (..), PlayerType (..), play, playerLevel, playerName, playerType)
import Core.UI (getSetupData)
import Control.Monad.State
import Lib.Printer (printChosenMove, printStateCurrent)
import Types.BotLevel (BotLevel (..))
import Types.Move (Move (..))
import Types.SetupData (SetupData (..))

data GameState = GameState
  { players :: [Player],
    board :: Board
  }
  deriving (Show)

type GameMonad = StateT GameState IO

newGameState :: Player -> Player -> Board -> GameState
newGameState player1 player2 board =
  GameState
    { players = [player1, player2],
      board = board
    }

playGame :: GameMonad ()
playGame = do
  gameState <- get
  if isGameOver (board gameState)
    then liftIO $ putStrLn "Jogo acabou"
    else do
      playRound (players gameState)
      playGame

playRound :: [Player] -> GameMonad ()
playRound [] = return ()
playRound (p : ps) = do
  gameState <- get
  if isGameOver (board gameState)
    then return ()
    else do
      playMove p
      playRound ps

playMove :: Player -> GameMonad ()
playMove player = do
  gameState <- get
  liftIO $ printStateCurrent (playerName player) (board gameState)
  move <- liftIO $ play player (board gameState)

  let updatedState = case move of
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

  liftIO $ printChosenMove move (playerName player) chosenDice

  if isGameOver (board updatedState)
    then do
      liftIO $ putStrLn $ playerName player ++ " venceu"
      put updatedState
    else put updatedState

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

  return initialState

game :: IO ()
game = do
  gameSetup <- getSetupData
  initialState <- buildGame gameSetup
  evalStateT playGame initialState