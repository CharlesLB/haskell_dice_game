module Lib.Reader (getUserBotLevel, displayPossibleRotations) where

import Core.Board.Dice (Dice (..), possibleRotations)
import Core.Players.BotPlayer (BotLevel (..))
import Data.Char (toLower)
import Lib.Printer (displayBotLevels, printDiceConfiguration)

getUserBotLevel :: IO BotLevel
getUserBotLevel = do
  displayBotLevels
  putStrLn "Digite o número correspondente ao nível desejado:"
  choice <- getLine
  case map toLower choice of
    "1" -> return Easy
    "2" -> return Hard
    _ -> do
      putStrLn "Opção inválida. Por favor, escolha uma opção válida (1 ou 2)."
      getUserBotLevel

displayPossibleRotations :: Dice -> IO Int
displayPossibleRotations chosenDice = do
  let rotations = possibleRotations chosenDice
  putStrLn "Possíveis rotações disponíveis:"
  mapM_ (\(i, option) -> putStrLn $ "- Girar para o valor: " ++ show option) (zip [1 ..] rotations)

  putStrLn "Digite para qual valor o dado deve ser girado:"
  newValue <- readLn
  if any (\x -> x == newValue) rotations
    then return newValue
    else do
      putStrLn "Opção inválida. Escolha uma das opções disponíveis."
      displayPossibleRotations chosenDice
