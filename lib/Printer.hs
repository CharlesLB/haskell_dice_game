module Lib.Printer (printStateCurrent) where

import Core.Dice (Dice)
import Core.Players.Player (Player(..))

printStateCurrent :: String -> [Dice] -> IO ()
printStateCurrent currentPlayer dices = do
    putStrLn "Estado atual do jogo:"
    putStrLn $ "Jogador atual: " ++ currentPlayer
    putStrLn "Dados:"
    printDiceConfiguration (dices)

printDiceConfiguration :: [Dice] -> IO ()
printDiceConfiguration dices = do
    putStrLn "Configuração dos Dados:"
    mapM_ printDice (zip [1..] dices)

printDice :: (Int, Int) -> IO ()
printDice (index, value) = putStrLn $ "Dado " ++ show index ++ ": " ++ show value