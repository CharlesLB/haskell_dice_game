module Lib.Printer (printStateCurrent, printChosenMove, printBotLevels, printDicesByValues, printMoveTypes, printPossibleRotationsOfDice, printDice) where

import Control.Monad (when)
import Core.Board.Board (Board (..))
import Core.Board.Dice (Dice (..))
import Types.Move (Index, Move (..), MoveType (..))

type PlayerName = String

printStateCurrent :: String -> Board -> IO ()
printStateCurrent currentPlayer board = do
  putStrLn $ "\n\n Jogador atual: " ++ currentPlayer
  printBoard board

printBoard :: Board -> IO ()
printBoard board = do
  putStrLn "Configuração Atual dos Dados:"
  mapM_ printDice (zip [0 ..] board)
  putStrLn "\n"

printDicesByValues :: [(Index, Int)] -> IO ()
printDicesByValues dices = do
  putStrLn "Dados disponíveis:"
  mapM_ printDiceByValue dices
  where
    printDiceByValue :: (Index, Int) -> IO ()
    printDiceByValue (index, value) = printDice (index, Dice value)

printDice :: (Index, Dice) -> IO ()
printDice (index, dice) = do
  putStr $ show (index + 1) ++ ") "
  printDiceImg (value dice)
  where
    printDiceImg :: Int -> IO ()
    printDiceImg n = putStrLn $
      case n of
        1 -> "_____\n  |     |\n  |  o  |\n  |_____|\n"
        2 -> "_____\n  |o    |\n  |     |\n  |____o|\n"
        3 -> "_____\n  |o    |\n  |  o  |\n  |____o|\n"
        4 -> "_____\n  |o   o|\n  |     |\n  |o___o|\n"
        5 -> "_____\n  |o   o|\n  |  o  |\n  |o___o|\n"
        6 -> "_____\n  |o   o|\n  |o   o|\n  |o___o|\n"
        _ -> "Invalid dice face"

printChosenMove :: Move -> PlayerName -> Dice -> IO ()
printChosenMove (UpdateMove index newValue) playerName chosenDice = do
  putStrLn $ playerName ++ " escolheu girar o dado " ++ show (index + 1) ++ " de valor " ++ show (value chosenDice) ++ " para " ++ show newValue
printChosenMove (RemoveMove index) playerName dice = do
  putStrLn $ playerName ++ " escolheu retirar o dado " ++ show (index + 1)

printBotLevels :: IO ()
printBotLevels = do
  putStrLn "Escolha o nível do bot:"
  putStrLn "1. Fácil"
  putStrLn "2. Difícil"

printMoveTypes :: [MoveType] -> IO ()
printMoveTypes moves = do
  putStrLn "Escolha a jogada a ser feita:"

  let canGirar = Update `elem` moves
      canRetirar = Remove `elem` moves

  when canGirar $ putStrLn "1. Girar"
  when canRetirar $ putStrLn "2. Retirar"

printPossibleRotationsOfDice :: [Int] -> IO ()
printPossibleRotationsOfDice rotations = do
  putStrLn "Possíveis rotações disponíveis:"
  mapM_ (\(i, option) -> putStrLn $ "- Girar para o valor: " ++ show option) (zip [0 ..] rotations)
