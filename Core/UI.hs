module Core.UI (getNumberOfDices, getNameHumanPlayer, getLevelBotPlayer, getPlayerMove) where

import Control.Monad (when)
import Core.Board.Board (Board (..), possibleDicesToRemovals, possibleDicesToRotations)
import Core.Board.Dice (Dice (..))
import Lib.Reader (displayPossibleRotations, getUserBotLevel)
import Types.BotLevel (BotLevel)
import Types.Move (Move (..))

getNumberOfDices :: IO Int
getNumberOfDices = do
  putStrLn "Quantos dados deseja jogar? "
  readLn

getNameHumanPlayer :: IO String
getNameHumanPlayer = do
  putStrLn "Qual o nome do jogador? "
  getLine

getLevelBotPlayer :: IO BotLevel
getLevelBotPlayer = do
  getUserBotLevel

getPlayerMove :: Board -> IO Move
getPlayerMove board = do
  putStrLn "Escolha a jogada a ser feita:"

  when (any (\dice -> value dice /= 1) board) $ putStrLn "1. Girar"
  when (any (\dice -> value dice == 1) board) $ putStrLn "2. Retirar"

  putStrLn "Digite o número correspondente à ação desejada:"

  -- Criar uma função no Reader para ler uma escolha de um conjunto de opções do TypeMove
  choicePlayer <- readLn
  case choicePlayer of
    1 ->
      if any (\dice -> value dice /= 1) board
        then do
          let dicesToRotations = possibleDicesToRotations board
          putStrLn "Possíveis rotações disponíveis:"
          mapM_ (\(i, option) -> putStrLn $ show i ++ ") " ++ show option) dicesToRotations
          putStrLn "Escolha o dado para girar:"
          index <- readLn
          if any (\(i, option) -> i == index) dicesToRotations
            then do
              let chosenDice = board !! index
              putStrLn $ "Dado escolhido: " ++ show (value chosenDice)
              newValue <- displayPossibleRotations chosenDice
              return (UpdateMove {updateIndex = index, newValue = newValue})
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
          mapM_ (\(i, option) -> putStrLn $ show i ++ ") " ++ show option) dicesToRemovals
          putStrLn "Escolha o dado para girar:"
          index <- readLn
          if any (\(i, option) -> i == index) dicesToRemovals
            then return (RemoveMove {removeIndex = index})
            else do
              putStrLn "Índice inválido. Tente novamente."
              getPlayerMove board
        else do
          putStrLn "Não há dados com face 1 para retirar."
          getPlayerMove board
    _ -> do
      putStrLn "Opção inválida. Tente novamente."
      getPlayerMove board
