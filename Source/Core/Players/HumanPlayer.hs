{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Eta reduce" #-}
module Source.Core.Players.HumanPlayer (HumanPlayer (..), initializeHumanPlayer, humanPlay) where

import Source.Core.Board.Board (Board (..), removeDiceByIndex, updateDiceByIndex)
import Source.Core.UI (getPlayerMove)
import Source.Types.Move (Move)

data HumanPlayer = HumanPlayer
  { humanName :: String
  }
  deriving (Show)

initializeHumanPlayer :: String -> IO HumanPlayer
initializeHumanPlayer nameHumanPlayer = return HumanPlayer {humanName = nameHumanPlayer}

humanPlay :: HumanPlayer -> Board -> IO Move
humanPlay player board = getPlayerMove board
