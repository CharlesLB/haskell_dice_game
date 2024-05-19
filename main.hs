module Main where

import Core.Game (game)

main :: IO ()
main = do
  putStrLn "Bem-vindo ao Jogo dos Dados!"
  game