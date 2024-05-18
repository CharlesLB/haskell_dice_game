module Core.Players.BotPlayer (BotPlayer (..), BotLevel (..), initializeBotPlayer, botPlay) where

import Core.Board.Board (Board)
import Core.Board.Dice (Dice (..), possibleRotations)
import System.Random (randomRIO)
import Types.BotLevel (BotLevel (..))
import Types.Move (Move (..))

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
    Hard -> easyBotMove gameState

easyBotMove :: Board -> IO Move
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
      return (UpdateMove {updateIndex = randomIndex, newValue = newValue})
    else do
      return (RemoveMove {removeIndex = randomIndex})
