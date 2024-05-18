module Main where

import Core.Game (initializingGame)

main :: IO ()
main = do
  putStrLn "Bem-vindo ao Jogo dos Dados!"
  initializingGame  