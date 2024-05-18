module Core.Game (newGameState, playGame, initializingGame) where

-- import System.Random
import Control.Monad (replicateM)
import Control.Monad.State
import Core.Dice (Dice (..), initializeDice, possibleRotations)
import Core.Players.BotPlayer (BotLevel (..), BotPlayer (..), initializeBotPlayer)
import Core.Players.HumanPlayer (HumanPlayer (..), initializeHumanPlayer)
import Core.Players.Player (Player (..), PlayerType (..))
import Core.UI (getPlayerMove, getLevelBoyPlayer, getNumberOfDices, getNameHumanPlayer)
import Lib.Printer (printChosenMove, printDiceConfiguration, printStateCurrent)
import System.Random

data GameState = GameState
  { humanPlayer :: HumanPlayer,
    botPlayer :: BotPlayer,
    dices :: [Dice],
    nextPlayer :: PlayerType
  }
  deriving (Show)

initializeDices :: Int -> IO [Dice]
initializeDices numDice = replicateM numDice initializeDice

newGameState :: HumanPlayer -> BotPlayer -> [Dice] -> PlayerType -> GameState
newGameState humanPlayer botPlayer dices nextPlayer =
  GameState
    { humanPlayer = humanPlayer,
      botPlayer = botPlayer,
      dices = dices,
      nextPlayer = nextPlayer
    }

-- updateNextPlayerGameState :: GameState -> PlayerType-> GameState
-- updateNextPlayerGameState gameState playerType = gameState { nextPlayer = playerType }

isGameOver :: GameState -> Bool
isGameOver gameState = null (dices gameState)

updateDiceByIndex :: [Dice] -> Int -> Int -> [Dice]
updateDiceByIndex [] _ _ = [] 
updateDiceByIndex (dice : dices) index newValue
  | index < 0 = dice : dices 
  | index == 0 = (Dice newValue) : dices 
  | otherwise = dice : updateDiceByIndex dices (index - 1) newValue

removeDiceByIndex :: [Dice] -> Int -> [Dice]
removeDiceByIndex [] _ = [] 
removeDiceByIndex (dice : dices) index
  | index < 0 = dice : dices 
  | index == 0 = dices
  | otherwise = dice : removeDiceByIndex dices (index - 1)

easyBotMove :: [Dice] -> IO (Int, Int, Int) --(choice, index, value)
easyBotMove diceList = do
  let numDices = length diceList
  randomIndex <- (randomRIO :: (Int, Int) -> IO Int) (0, numDices - 1)
  let chosenDice = diceList !! randomIndex
  if ((value chosenDice) /= 1)
    then do
      let rotations = possibleRotations chosenDice
      let n = length rotations
      indexDicesToRotation <- randomRIO (0, n - 1) -- Sorteia um índice aleatório
      let newValue = (rotations !! indexDicesToRotation)
      return (1, randomIndex, newValue)
    else do
      return (2, randomIndex, 0)

playGame :: GameState -> IO ()
playGame gameState
  | nextPlayer gameState == Human = do
    (choice, index, value) <- getPlayerMove (dices gameState)

    let actualizedState = case choice of
          1 ->
            let updatedDiceList = updateDiceByIndex (dices gameState) (index - 1) value
             in gameState {dices = updatedDiceList, nextPlayer = Bot}
          2 ->
            let updatedDiceList = removeDiceByIndex (dices gameState) (index - 1)
             in gameState {dices = updatedDiceList, nextPlayer = Bot}
    let chosenDice = dices gameState !! (index - 1)
    printChosenMove choice (playerName (humanPlayer actualizedState)) chosenDice index value
    printStateCurrent (playerName (humanPlayer actualizedState)) (dices actualizedState)
    if isGameOver actualizedState
      then putStrLn "Humano venceu"
      else playGame actualizedState
  | otherwise = do
    (choice, index, value) <- easyBotMove (dices gameState)

    let actualizedState = case choice of
          1 ->
            let updatedDiceList = updateDiceByIndex (dices gameState) index value
             in gameState {dices = updatedDiceList, nextPlayer = Human}
          2 ->
            let updatedDiceList = removeDiceByIndex (dices gameState) index
             in gameState {dices = updatedDiceList, nextPlayer = Human}

    let chosenDice = dices gameState !! (index)
    printChosenMove choice (playerName (botPlayer actualizedState)) chosenDice (index + 1) value
    printStateCurrent (playerName (botPlayer actualizedState)) (dices actualizedState)

    if isGameOver actualizedState
      then putStrLn "Bot venceu"
      else playGame actualizedState

initializingGame :: IO ()
initializingGame = do
  numDices <- getNumberOfDices
  dices <- initializeDices numDices
  printDiceConfiguration dices

  nameHumanPlayer <- getNameHumanPlayer
  human <- initializeHumanPlayer nameHumanPlayer
  putStrLn $ "O nome do jogador do tipo " ++ show (playerType human) ++ " é: " ++ playerName human

  levelBotPlayer <- getLevelBoyPlayer
  let nameBot = "Bot" ++ show levelBotPlayer
  
  bot <- initializeBotPlayer nameBot levelBotPlayer
  putStrLn $ "O nome do jogador do tipo " ++ show (playerType bot) ++ " é: " ++ playerName bot ++ ". Ele é do nivel " ++ show (botLevel bot)

  let initialState = case (botLevel bot) of
        Easy -> newGameState human bot dices Human
        Hard -> newGameState human bot dices Bot
  printStateCurrent (playerName (humanPlayer initialState)) (dices)
  playGame initialState

  return () -- Conclui a ação IO ()
