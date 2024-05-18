module Lib.Reader (getUserBotLevel, displayPossibleRotations) where

import Core.Board.Dice (Dice (..), possibleRotations)
import Data.Char (toLower)
import Lib.Printer (displayBotLevels, printDiceConfiguration)
import Text.Read (readMaybe)
import Types.BotLevel (BotLevel (..))

readInt :: Maybe Int -> Maybe Int -> IO Int
readInt Nothing Nothing = do
  input <- getLine
  maybe invalidInput return (readMaybe input)
  where
    invalidInput = do
      putStrLn "Input inválido. Tente novamente."
      readInt Nothing Nothing
readInt minBound maxBound = do
  input <- getLine
  case readMaybe input of
    Just n -> case (minBound, maxBound) of
      (Just minVal, Just maxVal) | n >= minVal && n <= maxVal -> return n
      (Just minVal, Nothing) | n >= minVal -> return n
      (Nothing, Just maxVal) | n <= maxVal -> return n
      _ -> invalidInput
    Nothing -> invalidInput
  where
    invalidInput = do
      putStrLn "Input inválido. Selecione uma opção válida."
      readInt minBound maxBound

getUserBotLevel :: IO BotLevel
getUserBotLevel = do
  displayBotLevels
  putStrLn "Digite o número correspondente ao nível desejado:"
  choice <- readInt (Just 1) (Just 2)
  case choice of
    1 -> return Easy
    2 -> return Hard
    _ -> getUserBotLevel

displayPossibleRotations :: Dice -> IO Int
displayPossibleRotations chosenDice = do
  let rotations = possibleRotations chosenDice
  putStrLn "Possíveis rotações disponíveis:"
  mapM_ (\(i, option) -> putStrLn $ "- Girar para o valor: " ++ show option) (zip [0 ..] rotations)

  putStrLn "Digite para qual valor o dado deve ser girado:"
  newValue <- readLn
  if any (\x -> x == newValue) rotations
    then return newValue
    else do
      putStrLn "Opção inválida. Escolha uma das opções disponíveis."
      displayPossibleRotations chosenDice
