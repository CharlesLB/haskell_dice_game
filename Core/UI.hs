module Core.UI (getNumberOfDices, getNameHumanPlayer, getLevelBoyPlayer, getPlayerMove) where

import Control.Monad.State
import Core.Dice (Dice (..))
import Core.Players.BotPlayer (BotLevel)
import Core.Players.HumanPlayer (HumanPlayer (..))
import Core.Players.Player (Player (..), PlayerType (..))
import Lib.Printer (printDiceConfiguration)
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

-- TODO mover lógica do jogo para o Game
possibleDicesToRemovals :: [Dice] -> [(Int, Int)]
possibleDicesToRemovals diceList =
  [(index, value dice) | (index, dice) <- zip [1 ..] diceList, value dice == 1]

-- showPossibleDiceToRemoval :: [Dice] -> IO ()
-- showPossibleDiceToRemoval dices = putStrLn

-- TODO mover lógica do jogo para o Game
possibleDicesToRotations :: [Dice] -> [(Int, Int)]
possibleDicesToRotations diceList =
  [(index, value dice) | (index, dice) <- zip [1 ..] diceList, value dice /= 1]

getPlayerMove :: [Dice] -> IO (Int, Int, Int)
getPlayerMove diceList = do
  putStrLn "Escolha a jogada a ser feita:"

  if any (\dice -> value dice /= 1) diceList
    then putStrLn "1. Girar"
    else return ()

  if any (\dice -> value dice == 1) diceList
    then putStrLn "2. Retirar"
    else return ()

  putStrLn "Digite o número correspondente à ação desejada:"
  choicePlayer <- readLn
  case choicePlayer of
    1 -> 
      if any (\dice -> value dice /= 1) diceList
        then do
          let dicesToRotations = possibleDicesToRotations diceList
          putStrLn "Possíveis rotações disponíveis:"
          mapM_ (\(i, option) -> putStrLn $ "Dado " ++ show i ++ ": " ++ show option) dicesToRotations
          putStrLn "Escolha o dado para girar:"
          index <- readLn
          if any (\(i, option) -> i == index) dicesToRotations
            then do
              let chosenDice = diceList !! (index - 1)
              putStrLn $ "Dado escolhido: " ++ show (value chosenDice)
              newValue <- displayPossibleRotations chosenDice
              return (1, index, newValue)
            else do
              putStrLn "Índice inválido. Tente novamente."
              getPlayerMove diceList
        else do
          putStrLn "Não há dados sem face 1 para girar."
          getPlayerMove diceList
    2 ->
      if any (\dice -> value dice == 1) diceList
        then do
          let dicesToRemovals = possibleDicesToRemovals diceList
          putStrLn "Possíveis remoções disponíveis:"
          mapM_ (\(i, option) -> putStrLn $ "Dado " ++ show i ++ ": " ++ show option) dicesToRemovals
          putStrLn "Escolha o dado para girar:"
          index <- readLn
          if any (\(i, option) -> i == index) dicesToRemovals
            then return (2, index, 0) -- Escolha 2 indica retirar
            else do
              putStrLn "Índice inválido. Tente novamente."
              getPlayerMove diceList
        else do
          putStrLn "Não há dados com face 1 para retirar."
          getPlayerMove diceList
    _ -> do
      putStrLn "Opção inválida. Tente novamente."
      getPlayerMove diceList
