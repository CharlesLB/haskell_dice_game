module Main where

import Lib.Reader (initializingDices, initializingPlayers)
-- import Core.Game (initializeGame, game)
-- import Core.Players.Player (Player(..), initializeHumanPlayer, initializeBotPlayer)

main :: IO ()
main = do
  putStrLn "Bem-vindo ao Jogo dos Dados!"
  initializingDices
  initializingPlayers