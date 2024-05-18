module Core.Game (newGameState, playGame, initializingGame) where

import Control.Monad (replicateM)
import Core.Board.Board (Board (..))
import Core.Board.Dice (Dice (..), initializeDice, possibleRotations)
import Core.Players.BotPlayer (BotLevel (..), BotPlayer (..), initializeBotPlayer)
import Core.Players.HumanPlayer (HumanPlayer (..), initializeHumanPlayer)
import Core.Players.Player (Player (..), PlayerType (..))
import Core.UI (getLevelBoyPlayer, getNameHumanPlayer, getNumberOfDices, getPlayerMove)
import Lib.Printer (printChosenMove, printDiceConfiguration, printStateCurrent)
import System.Random (randomRIO)

data GameState = GameState
  { humanPlayer :: HumanPlayer,
    botPlayer :: BotPlayer,
    board :: Board,
    nextPlayer :: PlayerType
  }
  deriving (Show)

initializeDices :: Int -> IO Board
initializeDices numDice = replicateM numDice initializeDice

newGameState :: HumanPlayer -> BotPlayer -> Board -> PlayerType -> GameState
newGameState humanPlayer botPlayer board nextPlayer =
  GameState
    { humanPlayer = humanPlayer,
      botPlayer = botPlayer,
      board = board,
      nextPlayer = nextPlayer
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

playGame :: GameState -> IO ()
playGame gameState
  | nextPlayer gameState == Human = do
      (choice, index, value) <- getPlayerMove (board gameState)

      let actualizedState = case choice of
            1 ->
              let updatedboard = updateDiceByIndex (board gameState) (index - 1) value
               in gameState {board = updatedboard, nextPlayer = Bot}
            2 ->
              let updatedboard = removeDiceByIndex (board gameState) (index - 1)
               in gameState {board = updatedboard, nextPlayer = Bot}
      let chosenDice = board gameState !! (index - 1)
      printChosenMove choice (playerName (humanPlayer actualizedState)) chosenDice index value
      printStateCurrent (playerName (humanPlayer actualizedState)) (board actualizedState)
      if isGameOver actualizedState
        then putStrLn "Humano venceu"
        else playGame actualizedState
  | otherwise = do
      (choice, index, value) <- easyBotMove (board gameState)

      let actualizedState = case choice of
            1 ->
              let updatedboard = updateDiceByIndex (board gameState) index value
               in gameState {board = updatedboard, nextPlayer = Human}
            2 ->
              let updatedboard = removeDiceByIndex (board gameState) index
               in gameState {board = updatedboard, nextPlayer = Human}

      let chosenDice = board gameState !! index
      printChosenMove choice (playerName (botPlayer actualizedState)) chosenDice (index + 1) value
      printStateCurrent (playerName (botPlayer actualizedState)) (board actualizedState)

      if isGameOver actualizedState
        then putStrLn "Bot venceu"
        else playGame actualizedState

initializingGame :: IO ()
initializingGame = do
  numDices <- getNumberOfDices
  board <- initializeDices numDices
  printDiceConfiguration board

  nameHumanPlayer <- getNameHumanPlayer
  human <- initializeHumanPlayer nameHumanPlayer
  putStrLn $ "O nome do jogador do tipo " ++ show (playerType human) ++ " é: " ++ playerName human

  levelBotPlayer <- getLevelBoyPlayer
  let nameBot = "Bot" ++ show levelBotPlayer

  bot <- initializeBotPlayer nameBot levelBotPlayer
  putStrLn $ "O nome do jogador do tipo " ++ show (playerType bot) ++ " é: " ++ playerName bot ++ ". Ele é do nivel " ++ show (botLevel bot)

  let initialState = case botLevel bot of
        Easy -> newGameState human bot board Human
        Hard -> newGameState human bot board Bot
  printStateCurrent (playerName (humanPlayer initialState)) board
  playGame initialState

  return () -- Conclui a ação IO ()
