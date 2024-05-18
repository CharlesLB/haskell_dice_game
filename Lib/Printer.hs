module Lib.Printer (printStateCurrent, printDiceConfiguration, printChosenMove, displayBotLevels) where

import Core.Board.Dice (Dice (..))

printStateCurrent :: String -> [Dice] -> IO ()
printStateCurrent currentPlayer board = do
  putStrLn "Estado atual do jogo:"
  putStrLn $ "Jogador atual: " ++ currentPlayer
  printDiceConfiguration board

printDice :: (Int, Dice) -> IO ()
printDice (index, dice) = putStrLn $ "Dado " ++ show index ++ ": " ++ show (value dice)

printDiceConfiguration :: [Dice] -> IO ()
printDiceConfiguration board = do
  putStrLn "Configuração Atual dos Dados:"
  mapM_ printDice (zip [1 ..] board)
  putStrLn "\n"

printChosenMove :: Int -> String -> Dice -> Int -> Int -> IO ()
printChosenMove choice name chosenDice index newValue = do
  if choice == 1
    then
      putStrLn $
        name
          ++ " escolheu girar o dado "
          ++ show index
          ++ " de valor "
          ++ show (value chosenDice)
          ++ " para "
          ++ show newValue
    else putStrLn $ name ++ " escolheu retirar o dado " ++ show index

displayBotLevels :: IO ()
displayBotLevels = do
  putStrLn "Escolha o nível do bot:"
  putStrLn "1. Fácil"
  putStrLn "2. Difícil"