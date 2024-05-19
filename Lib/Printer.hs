module Lib.Printer (printStateCurrent, printBoard, printChosenMove, printBotLevels, printDicesByValues, printMoveTypes, printPossibleRotationsOfDice) where

import Control.Monad (when)
import Core.Board.Board (Board (..))
import Core.Board.Dice (Dice (..))
import Types.Move (Move (RemoveMove, UpdateMove), MoveType (Remove, Update))

type PlayerName = String

printStateCurrent :: String -> Board -> IO ()
printStateCurrent currentPlayer board = do
  putStrLn "Sua vez:"
  putStrLn $ "Jogador atual: " ++ currentPlayer
  printBoard board

printBoard :: Board -> IO ()
printBoard board = do
  putStrLn "Configuração Atual dos Dados:"
  mapM_ printDice (zip [1 ..] board)
  putStrLn "\n"

printDicesByValues :: [(Int, Int)] -> IO ()
printDicesByValues dices = do
  putStrLn "Dados disponíveis:"
  mapM_ printDiceByValue dices
  where
    printDiceByValue :: (Int, Int) -> IO ()
    printDiceByValue (index, value) = printDice (index, Dice value)

printDice :: (Int, Dice) -> IO ()
printDice (index, dice) = do
  putStr $ show index ++ ") "
  printDiceImg (value dice)
  where
    printDiceImg :: Int -> IO ()
    printDiceImg n = putStrLn $
      case n of
        1 -> "_____\n  |     |\n  |  ●  |\n  |_____|\n"
        2 -> "_____\n  |●    |\n  |     |\n  |____●|\n"
        3 -> "_____\n  |●    |\n  |  ●  |\n  |____●|\n"
        4 -> "_____\n  |●   ●|\n  |     |\n  |●___●|\n"
        5 -> "_____\n  |●   ●|\n  |  ●  |\n  |●___●|\n"
        6 -> "_____\n  |●   ●|\n  |●   ●|\n  |●___●|\n"
        _ -> "Invalid dice face"

printChosenMove :: Move -> PlayerName -> Dice -> IO ()
printChosenMove (UpdateMove index newValue) playerName chosenDice = do
  putStrLn $ playerName ++ " escolheu girar o dado " ++ show index ++ " de valor " ++ show (value chosenDice) ++ " para " ++ show newValue
printChosenMove (RemoveMove index) playerName dice = do
  putStrLn $ playerName ++ " escolheu retirar o dado " ++ show index

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
