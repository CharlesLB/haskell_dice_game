module Source.Core.Players.BotPlayer (BotPlayer (..), BotLevel (..), initializeBotPlayer, botPlay, hardBotMove) where

import Data.List (delete)
import Source.Core.Board.Board (Board)
import Source.Core.Board.Dice (Dice (..), possibleRotations)
import Source.Types.BotLevel (BotLevel (..))
import Source.Types.Move (Move (..))
import System.Random (randomRIO)

data BotPlayer = BotPlayer
  { botName :: String,
    botLevel :: BotLevel
  }
  deriving (Show)

initializeBotPlayer :: String -> BotLevel -> IO BotPlayer
initializeBotPlayer nameBotPlayer levelBotPlayer = do
  let bot = BotPlayer {botName = nameBotPlayer, botLevel = levelBotPlayer}
  return bot

botPlay :: BotPlayer -> Board -> IO Move
botPlay player gameState = do
  case botLevel player of
    Easy -> easyBotMove gameState
    Hard -> hardBotMove gameState

easyBotMove :: Board -> IO Move
easyBotMove board = do
  let numDices = length board
  randomIndex <- (randomRIO :: (Int, Int) -> IO Int) (0, numDices - 1)
  let chosenDice = board !! randomIndex
  if value chosenDice /= 1
    then do
      let rotations = possibleRotations chosenDice
      indexDicesToRotation <- randomRIO (0, length rotations - 1)
      let newValue = rotations !! indexDicesToRotation
      return (UpdateMove {updateIndex = randomIndex, newValue = newValue})
    else do
      return (RemoveMove {removeIndex = randomIndex})

hardBotMove :: Board -> IO Move
hardBotMove board = do
  let potentialMoves = [(i, r) | i <- [0 .. length board - 1], r <- possibleRotations (board !! i)]
  let winningMoves = [(i, r) | (i, r) <- potentialMoves, isLoser (updateBoard board i r)]
  let removeMoves = [i | i <- [0 .. length board - 1], value (board !! i) == 1]

  case (winningMoves, removeMoves) of
    ((i, r) : _, _) -> return $ UpdateMove {updateIndex = i, newValue = r}
    (_, i : _) -> return $ RemoveMove {removeIndex = i}
    _ -> easyBotMove board
  where
    isWinner :: Board -> Bool
    isWinner board = not (isLoser board)

    isLoser :: Board -> Bool
    isLoser board = case board of
      [] -> False
      [dice] -> value dice `elem` [2, 5]
      [dice1, dice2]
        | value dice1 == value dice2 -> True
        | value dice1 + value dice2 == 7 -> True
        | otherwise -> False
      _ -> all (\dice -> value dice == 2 || value dice == 5) board || checkPairs board

    checkPairs :: Board -> Bool
    checkPairs [] = True
    checkPairs [x] = False
    checkPairs (x : xs) = any (checkPair x xs) (zip [0 ..] xs) || checkPairs xs

    checkPair :: Dice -> Board -> (Int, Dice) -> Bool
    checkPair x xs (i, y) =
      isLoser [x, y] && isLoser (take i xs ++ drop (i + 1) xs)

    updateBoard :: Board -> Int -> Int -> Board
    updateBoard board index newValue =
      take index board ++ [Dice {value = newValue}] ++ drop (index + 1) board
