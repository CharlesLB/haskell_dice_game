{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Core.Players.HumanPlayer (HumanPlayer (..), initializeHumanPlayer, humanPlay) where

import Core.Board.Board (Board)
import Core.UI (getPlayerMove)

data HumanPlayer = HumanPlayer
  { humanName :: String
  }
  deriving (Show)

initializeHumanPlayer :: String -> IO HumanPlayer
initializeHumanPlayer nameHumanPlayer = return HumanPlayer {humanName = nameHumanPlayer}

humanPlay :: HumanPlayer -> Board -> IO ()
humanPlay player board = do
  (choice, index, value) <- getPlayerMove board
  print "po"
