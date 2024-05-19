module Lib.Reader (readInt, readMoveType, readBotLevel, readIndex, readString, readDiceByList, readNewDiceValue) where

import Control.Monad (when)
import Core.Board.Board (Board)
import Core.Board.Dice (Dice (..), possibleRotations)
import Data.Char (toLower)
import Lib.Printer (printBotLevels, printDicesByValues, printMoveTypes, printPossibleRotationsOfDice)
import Text.Read (readMaybe)
import Types.BotLevel (BotLevel (..))
import Types.Move (MoveType (Remove, Update))

readInt :: Maybe Int -> Maybe Int -> IO Int
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

readIndex :: Maybe Int -> Maybe Int -> IO Int
readIndex minBound maxBound = do
  input <- readInt minBound maxBound
  return (input - 1)

readString :: IO String
readString = getLine

readBotLevel :: IO BotLevel
readBotLevel = do
  printBotLevels
  putStrLn "Digite o número correspondente ao nível desejado:"
  choice <- readInt (Just 1) (Just 2)
  case choice of
    1 -> return Easy
    2 -> return Hard
    _ -> readBotLevel

readMoveType :: Board -> IO MoveType
readMoveType board = do
  let possibleMoveTypes = [Update | any (\dice -> value dice /= 1) board] ++ [Remove | any (\dice -> value dice == 1) board]
  putStrLn "Escolha a jogada a ser feita:"

  printMoveTypes possibleMoveTypes

  putStrLn "Qual jogada deseja fazer:"
  choice <- readInt (Just 1) (Just 2)

  let selectedMoveType = case choice of
        1 -> Update
        2 -> Remove
        _ -> error "Escolha inválida"

  if selectedMoveType `elem` possibleMoveTypes
    then return selectedMoveType
    else do
      putStrLn "Jogada inválida. Tente novamente."
      readMoveType board

readDiceByList :: [(Int, Int)] -> String -> IO Int
readDiceByList dices message = do
  printDicesByValues dices
  putStrLn message
  index <- readIndex (Just 1) Nothing

  if any (\(i, option) -> i == index) dices
    then return index
    else do
      putStrLn "Índice inválido. Tente novamente."
      readDiceByList dices message

readNewDiceValue :: Dice -> IO Int
readNewDiceValue dice = do
  let rotations = possibleRotations dice
  printPossibleRotationsOfDice rotations

  putStrLn "Digite para qual valor o dado deve ser girado:"
  newValue <- readLn
  if newValue `elem` rotations
    then return newValue
    else do
      putStrLn "Opção inválida. Escolha uma das opções disponíveis."
      readNewDiceValue dice