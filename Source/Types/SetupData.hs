module Source.Types.SetupData where

import Source.Types.BotLevel (BotLevel)

data SetupData = SetupData
  { numDices :: Int,
    setupPlayerName :: String,
    setupBotLevel :: BotLevel
  }
  deriving (Show)