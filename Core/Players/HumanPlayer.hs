module Core.Players.HumanPlayer (HumanPlayer(..)) where

data HumanPlayer = HumanPlayer
  { humanName :: String
  }
  deriving (Show)