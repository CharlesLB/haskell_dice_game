module Lib.Printer (printStateCurrent, printDiceConfiguration, printChosenMove) where

import Core.Dice (Dice(..))
import Core.Players.Player (Player(..))

printStateCurrent :: String -> [Dice] -> IO ()
printStateCurrent currentPlayer dices = do
    putStrLn "Estado atual do jogo:"
    putStrLn $ "Jogador atual: " ++ currentPlayer
    printDiceConfiguration (dices)

printDice :: (Int, Dice) -> IO ()
printDice (index, dice) = putStrLn $ "Dado " ++ show index ++ ": " ++ show (value dice)

printDiceConfiguration :: [Dice] -> IO ()
printDiceConfiguration dices = do
    putStrLn "Configuração Atual dos Dados:"
    mapM_ printDice (zip [1..] dices)

printChosenMove :: String -> Dice -> Int -> Int -> IO ()
printChosenMove humanName chosenDice index action = do
    putStrLn $ humanName ++ " escolheu girar o dado " ++ show index ++
                                                        " de valor " ++ show (value chosenDice) ++
                                                        " para " ++ show action