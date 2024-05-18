module Core.Players.Player (Player (..), PlayerType (..)) where

import Core.Players.BotPlayer (BotLevel (..), BotPlayer (..))
import Core.Players.HumanPlayer (HumanPlayer (..))

data PlayerType = Human | Bot
  deriving (Show, Eq)

class Player a where
  playerName :: a -> String
  playerType :: a -> PlayerType

instance Player HumanPlayer where
  playerName :: HumanPlayer -> String
  playerName = humanName

  playerType :: HumanPlayer -> PlayerType
  playerType _ = Human

instance Player BotPlayer where
  playerName :: BotPlayer -> String
  playerName = botName

  playerType :: BotPlayer -> PlayerType
  playerType _ = Bot
