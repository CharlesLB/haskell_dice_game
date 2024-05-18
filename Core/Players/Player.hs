module Core.Players.Player (Player (..), PlayerType (..), playerName, playerType, play, playerLevel) where

import Core.Board.Board (Board)
import Core.Players.BotPlayer (BotLevel (..), BotPlayer (..), botPlay)
import Core.Players.HumanPlayer (HumanPlayer (..), humanPlay)

data PlayerType = Human | Bot
  deriving (Show, Eq)

data Player
  = HumanPlayerType HumanPlayer
  | BotPlayerType BotPlayer
  deriving (Show)

playerName :: Player -> String
playerName (HumanPlayerType hp) = humanName hp
playerName (BotPlayerType bp) = botName bp

playerType :: Player -> PlayerType
playerType (HumanPlayerType _) = Human
playerType (BotPlayerType _) = Bot

play :: Player -> Board -> IO ()
play (HumanPlayerType hp) board = humanPlay hp board
play (BotPlayerType bp) board = botPlay bp board

playerLevel :: Player -> Maybe BotLevel
playerLevel (BotPlayerType bp) = Just (botLevel bp)
playerLevel _ = Nothing
