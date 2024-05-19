module Types.SetupData where

import Types.BotLevel (BotLevel)

data SetupData = SetupData
  { numDices :: Int,
    setupPlayerName :: String,
    setupBotLevel :: BotLevel
  }
  deriving (Show)