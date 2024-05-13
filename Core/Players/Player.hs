module Core.Players.Player (Player(..), initializeHumanPlayer, initializeBotPlayer) where

import Core.Players.HumanPlayer (HumanPlayer(..))
import Core.Players.BotPlayer (BotPlayer(..))

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

initializeHumanPlayer :: String -> IO HumanPlayer
initializeHumanPlayer nameHumanPlayer = do
  let human = HumanPlayer { humanName = nameHumanPlayer }
  return human

initializeBotPlayer :: String -> IO BotPlayer
initializeBotPlayer nameBotPlayer = do
  let bot = BotPlayer { botName = nameBotPlayer }
  return bot