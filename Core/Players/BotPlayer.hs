module Core.Players.BotPlayer (BotPlayer (..), BotLevel (..), initializeBotPlayer) where

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