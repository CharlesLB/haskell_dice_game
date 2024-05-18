{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Core.Players.HumanPlayer (HumanPlayer (..), initializeHumanPlayer, humanPlay) where

import Data.Dynamic (Dynamic)

data HumanPlayer = HumanPlayer
  { humanName :: String
  }
  deriving (Show)

initializeHumanPlayer :: String -> IO HumanPlayer
initializeHumanPlayer nameHumanPlayer = return HumanPlayer {humanName = nameHumanPlayer}

humanPlay :: HumanPlayer -> gameState -> IO ()
humanPlay player gameState = print "Human"