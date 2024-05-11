{-# LANGUAGE InstanceSigs #-}

class Player a where
  playerName :: a -> String
  playerType :: a -> String
  play :: a -> IO ()

data HumanPlayer = HumanPlayer
  { humanName :: String
  }
  deriving (Show)

instance Player HumanPlayer where
  playerName :: HumanPlayer -> String
  playerName = humanName
  playerType :: HumanPlayer -> String
  playerType _ = "Human"
  play :: HumanPlayer -> IO ()
  play human = putStrLn ("Player " ++ playerName human ++ " is making a move.")

data BotPlayer = BotPlayer
  { botName :: String
  }
  deriving (Show)

instance Player BotPlayer where
  playerName :: BotPlayer -> String
  playerName = botName

  playerType :: BotPlayer -> String
  playerType _ = "Bot"

  play :: BotPlayer -> IO ()
  play bot = putStrLn ("Bot " ++ playerName bot ++ " is making a move.")

main :: IO ()
main = do
  let human = HumanPlayer {humanName = "Alice"}
  let bot = BotPlayer {botName = "AI Bot"}

  print (playerType human)
  play bot
