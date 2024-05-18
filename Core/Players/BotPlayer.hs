module Core.Players.BotPlayer (BotPlayer (..), BotLevel (..), initializeBotPlayer) where

import Core.Dice (Dice (..))

data BotLevel = Easy | Hard
  deriving (Show, Eq)

data BotPlayer = BotPlayer
  { botName :: String,
    botLevel :: BotLevel
  }
  deriving (Show)

initializeBotPlayer :: String -> BotLevel -> IO BotPlayer
initializeBotPlayer nameBotPlayer levelBotPlayer = do
  let bot = BotPlayer {botName = nameBotPlayer, botLevel = levelBotPlayer}
  return bot