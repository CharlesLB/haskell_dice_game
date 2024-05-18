module Core.Players.BotPlayer (BotPlayer (..), BotLevel (..), initializeBotPlayer, botPlay) where

import Core.Board.Dice (Dice (..))
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

botPlay :: BotPlayer -> gameState -> IO ()
botPlay player gameState = do
  putStrLn $ "Bot Level: " ++ show (botLevel player)
