module Core.Players.BotPlayer (BotPlayer(..)) where

data BotLevel = Easy | Medium | Hard
  deriving (Show, Eq)

data BotPlayer = BotPlayer
  { botName :: String
  }
  deriving (Show)