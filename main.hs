module Main where

import Core.Game (game)

main :: IO ()
main = do
  putStrLn "Welcome to the Dice Game!"
  game
