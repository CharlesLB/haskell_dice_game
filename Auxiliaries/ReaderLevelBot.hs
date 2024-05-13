module Auxiliaries.ReaderLevelBot (getUserBotLevel) where

import Core.Players.BotPlayer (BotLevel(..))

import Data.Char (toLower)

displayBotLevels :: IO ()
displayBotLevels = do
  putStrLn "Escolha o nível do bot:"
  putStrLn "1. Fácil"
  putStrLn "2. Médio"
  putStrLn "3. Difícil"

getUserBotLevel :: IO BotLevel
getUserBotLevel = do
  displayBotLevels
  putStrLn "Digite o número correspondente ao nível desejado:"
  choice <- getLine
  case map toLower choice of
    "1" -> return Easy
    "2" -> return Medium
    "3" -> return Hard
    _   -> do
      putStrLn "Opção inválida. Por favor, escolha uma opção válida (1, 2 ou 3)."
      getUserBotLevel