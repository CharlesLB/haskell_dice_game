module Core.Players.Player (Player(..)) where

import Core.Players.HumanPlayer (HumanPlayer(..))
import Core.Players.BotPlayer (BotPlayer(..), BotLevel(..))

class Player a where
  playerName :: a -> String
  playerType :: a -> String
  -- play :: a -> IO ()

instance Player HumanPlayer where
  playerName :: HumanPlayer -> String
  playerName = humanName

  playerType :: HumanPlayer -> String
  playerType _ = "Human"

instance Player BotPlayer where
  playerName :: BotPlayer -> String
  playerName = botName

  playerType :: BotPlayer -> String
  playerType _ = "Bot"

