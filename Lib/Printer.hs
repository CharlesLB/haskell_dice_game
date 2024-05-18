module Lib.Printer (printStateCurrent, printDiceConfiguration, printChosenMove, displayBotLevels) where

import Core.Board.Board (Board)
import Core.Board.Dice (Dice (..))
import Types.Move (Move (RemoveMove, UpdateMove))

type PlayerName = String

printStateCurrent :: String -> Board -> IO ()
printStateCurrent currentPlayer board = do
  putStrLn "Estado atual do jogo:"
  putStrLn $ "Jogador atual: " ++ currentPlayer
  printDiceConfiguration board

printDice :: (Int, Dice) -> IO ()
printDice (index, dice) = putStrLn $ "Dado " ++ show index ++ ": " ++ show (value dice)

printDiceConfiguration :: Board -> IO ()
printDiceConfiguration board = do
  putStrLn "Configuração Atual dos Dados:"
  mapM_ printDice (zip [0 ..] board)
  putStrLn "\n"

printChosenMove :: Move -> PlayerName -> Dice -> IO ()
printChosenMove (UpdateMove index newValue) playerName chosenDice = do
  putStrLn $ playerName ++ " escolheu girar o dado " ++ show index ++ " de valor " ++ show (value chosenDice) ++ " para " ++ show newValue
printChosenMove (RemoveMove index) playerName dice = do
  putStrLn $ playerName ++ " escolheu retirar o dado " ++ show index

displayBotLevels :: IO ()
displayBotLevels = do
  putStrLn "Escolha o nível do bot:"
  putStrLn "1. Fácil"
  putStrLn "2. Difícil"