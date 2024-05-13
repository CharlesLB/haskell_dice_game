module Core.Players.BotPlayer (BotPlayer(..), BotLevel(..)) where

data BotLevel = Easy | Medium | Hard
  deriving (Show, Eq)

data BotPlayer = BotPlayer
  { botName :: String,
  	botLevel :: BotLevel
  }
  deriving (Show)