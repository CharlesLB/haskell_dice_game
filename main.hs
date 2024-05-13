module Main where

import Lib.Reader (initializingDices, initializingPlayers)

main :: IO ()
main = do
  putStrLn "Bem-vindo ao Jogo dos Dados!"
  initializingDices
  initializingPlayers