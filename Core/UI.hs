module Core.UI (getNumberOfDices, getNameHumanPlayer, getLevelBoyPlayer, getPlayerMove) where

import Control.Monad.State
import Core.Dice (Dice (..))
import Core.Board (Board (..), possibleDicesToRotations, possibleDicesToRemovals)
import Core.Players.BotPlayer (BotLevel)
import Lib.Reader (displayPossibleRotations, getUserBotLevel)

getNumberOfDices :: IO Int
getNumberOfDices = do
  putStrLn "Quantos dados deseja jogar? "
  numDice <- readLn
  return numDice

getNameHumanPlayer :: IO String
getNameHumanPlayer = do
  putStrLn "Qual o nome do jogador? "
  nameHumanPlayer <- getLine
  return nameHumanPlayer

getLevelBoyPlayer :: IO BotLevel
getLevelBoyPlayer = do
  levelBotPlayer <- getUserBotLevel
  return levelBotPlayer

getPlayerMove :: Board -> IO (Int, Int, Int)
getPlayerMove board = do
  putStrLn "Escolha a jogada a ser feita:"

  if any (\dice -> value dice /= 1) board
    then putStrLn "1. Girar"
    else return ()

  if any (\dice -> value dice == 1) board
    then putStrLn "2. Retirar"
    else return ()

  putStrLn "Digite o número correspondente à ação desejada:"
  choicePlayer <- readLn
  case choicePlayer of
    1 -> 
      if any (\dice -> value dice /= 1) board
        then do
          let dicesToRotations = possibleDicesToRotations board
          putStrLn "Possíveis rotações disponíveis:"
          mapM_ (\(i, option) -> putStrLn $ "Dado " ++ show i ++ ": " ++ show option) dicesToRotations
          putStrLn "Escolha o dado para girar:"
          index <- readLn
          if any (\(i, option) -> i == index) dicesToRotations
            then do
              let chosenDice = board !! (index - 1)
              putStrLn $ "Dado escolhido: " ++ show (value chosenDice)
              newValue <- displayPossibleRotations chosenDice
              return (1, index, newValue)
            else do
              putStrLn "Índice inválido. Tente novamente."
              getPlayerMove board
        else do
          putStrLn "Não há dados sem face 1 para girar."
          getPlayerMove board
    2 ->
      if any (\dice -> value dice == 1) board
        then do
          let dicesToRemovals = possibleDicesToRemovals board
          putStrLn "Possíveis remoções disponíveis:"
          mapM_ (\(i, option) -> putStrLn $ "Dado " ++ show i ++ ": " ++ show option) dicesToRemovals
          putStrLn "Escolha o dado para girar:"
          index <- readLn
          if any (\(i, option) -> i == index) dicesToRemovals
            then return (2, index, 0) -- Escolha 2 indica retirar
            else do
              putStrLn "Índice inválido. Tente novamente."
              getPlayerMove board
        else do
          putStrLn "Não há dados com face 1 para retirar."
          getPlayerMove board
    _ -> do
      putStrLn "Opção inválida. Tente novamente."
      getPlayerMove board
