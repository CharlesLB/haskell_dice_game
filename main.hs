module Main where

import Lib.Reader (initializingGame)

main :: IO ()
main = do
  putStrLn "Bem-vindo ao Jogo dos Dados!"
  initializingGame