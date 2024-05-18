module Core.Players.BotPlayer (BotPlayer (..), BotLevel (..), initializeBotPlayer, botPlay) where

import Core.Board.Board (Board)
import Core.Board.Dice (Dice (..), possibleRotations)
import System.Random (randomRIO)
import Types.BotLevel (BotLevel (..))

data BotPlayer = BotPlayer
  { botName :: String,
    botLevel :: BotLevel
  }
  deriving (Show)

initializeBotPlayer :: String -> BotLevel -> IO BotPlayer
initializeBotPlayer nameBotPlayer levelBotPlayer = do
  let bot = BotPlayer {botName = nameBotPlayer, botLevel = levelBotPlayer}
  return bot

botPlay :: BotPlayer -> Board -> IO ()
botPlay player gameState = do
  putStrLn $ "Bot Level: " ++ show (botLevel player)

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
