module Core.Players.HumanPlayer (HumanPlayer(..), initializeHumanPlayer) where

data HumanPlayer = HumanPlayer
  { humanName :: String
  }
  deriving (Show)

initializeHumanPlayer :: String -> IO HumanPlayer
initializeHumanPlayer nameHumanPlayer = do
  let human = HumanPlayer { humanName = nameHumanPlayer }
  return human