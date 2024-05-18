module Core.Game (newGameState, playGame, initializingGame) where

import Core.Dice (Dice(..), possibleRotations)
import Core.Players.Player (Player(..), PlayerType(..))
import Core.Players.HumanPlayer (HumanPlayer(..))
import Core.Players.BotPlayer (BotPlayer(..), BotLevel(..))
import Core.UI (initializingDices, initializingHumanPlayer, initializingBotPlayer, getPlayerMove)
import Lib.Printer (printStateCurrent, printChosenMove, printDiceConfiguration)

import System.Random

-- import System.Random
import Control.Monad (replicateM)
import Control.Monad.State

data GameState = GameState
  { humanPlayer :: HumanPlayer, 
    botPlayer :: BotPlayer,
    dices :: [Dice], 
    nextPlayer :: PlayerType
  } deriving (Show)

-- TODO: Não alocar nova memória on update
newGameState :: HumanPlayer -> BotPlayer -> [Dice] -> PlayerType -> GameState
newGameState humanPlayer botPlayer dices nextPlayer = GameState
    { humanPlayer = humanPlayer
    , botPlayer = botPlayer
    , dices = dices 
    , nextPlayer = nextPlayer
    }

-- updateNextPlayerGameState :: GameState -> PlayerType-> GameState
-- updateNextPlayerGameState gameState playerType = gameState { nextPlayer = playerType }

isGameOver :: GameState -> Bool
isGameOver gameState = null (dices gameState)

-- type GameMonad a = StateT GameState IO a

updateDiceByIndex :: [Dice] -> Int -> Int -> [Dice]
updateDiceByIndex [] _ _ = []  -- Caso base: lista vazia
updateDiceByIndex (dice:dices) index newValue
    | index < 0 = dice:dices  -- Se o índice for menor que 0, retornar a lista original
    | index == 0 = (Dice newValue):dices  -- Atualizar o dado no índice 0
    | otherwise = dice : updateDiceByIndex dices (index - 1) newValue

removeDiceByIndex :: [Dice] -> Int -> [Dice]
removeDiceByIndex [] _ = []  -- Se a lista estiver vazia, retorna uma lista vazia
removeDiceByIndex (dice:dices) index
    | index < 0 = dice:dices  -- Se o índice for menor que 0, retorna a lista original
    | index == 0 = dices  -- Se o índice for 0, remove o primeiro dado da lista
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
            indexDicesToRotation <- randomRIO (0, n - 1)  -- Sorteia um índice aleatório
            let newValue = (rotations !! indexDicesToRotation)
            return (1, randomIndex, newValue)
        else do
            return (2, randomIndex, 0)


playGame :: GameState -> IO ()
playGame gameState
    | nextPlayer gameState == Human = do
        (choice, index, value) <- getPlayerMove (dices gameState)

        let actualizedState = case choice of
                1 -> let updatedDiceList = updateDiceByIndex (dices gameState) (index - 1) value
                     in gameState { dices = updatedDiceList, nextPlayer = Bot }
                2 -> let updatedDiceList = removeDiceByIndex (dices gameState) (index - 1) 
                     in gameState { dices = updatedDiceList, nextPlayer = Bot }
        let chosenDice = dices gameState !! (index - 1)
        printChosenMove choice (playerName (humanPlayer actualizedState)) chosenDice index value
        printStateCurrent (playerName (humanPlayer actualizedState)) (dices actualizedState)
        if isGameOver actualizedState
            then putStrLn "Humano venceu"
        else playGame actualizedState

    | otherwise = do
        putStrLn "- Vez do bot"
        (choice, index, value) <- easyBotMove (dices gameState)

        let actualizedState = case choice of
                1 -> let updatedDiceList = updateDiceByIndex (dices gameState) index value 
                     in gameState { dices = updatedDiceList, nextPlayer = Human }
                2 -> let updatedDiceList = removeDiceByIndex (dices gameState) index 
                     in gameState { dices = updatedDiceList, nextPlayer = Human }

        let chosenDice = dices gameState !! (index)
        printChosenMove choice (playerName (botPlayer actualizedState)) chosenDice (index + 1) value
        printStateCurrent (playerName (botPlayer actualizedState)) (dices actualizedState)

        if isGameOver actualizedState
            then putStrLn "Bot venceu"
        else playGame actualizedState


initializingGame :: IO ()
initializingGame = do
  dices <- initializingDices
  human <- initializingHumanPlayer
  bot <- initializingBotPlayer

  let initialState = case (botLevel bot) of
                Easy -> newGameState human bot dices Human
                Hard -> newGameState human bot dices Bot
  printStateCurrent (playerName (humanPlayer initialState)) (dices)
  playGame initialState

  return ()  -- Conclui a ação IO ()


