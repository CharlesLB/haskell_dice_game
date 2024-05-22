module Source.Main where

import Source.Core.Game (game)

main :: IO ()
main = do
  putStrLn "Bem-vindo ao Jogo dos Dados!"
  game