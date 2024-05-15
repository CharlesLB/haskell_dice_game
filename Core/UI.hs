module Core.UI (initializingDices, initializingHumanPlayer, initializingBotPlayer, getPlayerMove) where

import Core.Dice (Dice(..), initializeDices)
import Core.Players.Player (Player(..), PlayerType(..))
import Core.Players.HumanPlayer (HumanPlayer(..), initializeHumanPlayer) 
import Core.Players.BotPlayer (BotPlayer(..), BotPlayer(..), initializeBotPlayer)
import Lib.Reader (getUserBotLevel, displayPossibleRotations) 

import Control.Monad.State

initializingDices :: IO [Dice]
initializingDices = do 
  putStrLn "Quantos dados deseja jogar? "
  numDice <- readLn
  dices <- initializeDices numDice
  putStrLn $ "Configuração inicial dos dados: " ++ show dices

  return dices 

initializingHumanPlayer :: IO HumanPlayer
initializingHumanPlayer = do
  putStrLn "Qual o nome do jogador? "
  nameHumanPlayer <- getLine
  human <- initializeHumanPlayer nameHumanPlayer 

  putStrLn $ "O nome do jogador do tipo " ++ show (playerType human) ++ " é: " ++ playerName human
  return human

initializingBotPlayer :: IO BotPlayer
initializingBotPlayer = do
  putStrLn "Qual o nome do bot? "
  nameBotPlayer <- getLine
  levelBotPlayer <- getUserBotLevel
  bot <- initializeBotPlayer nameBotPlayer levelBotPlayer

  putStrLn $ "O nome do jogador do tipo " ++ show (playerType bot) ++ " é: " ++ playerName bot ++ ". Ele é do nivel " ++ show (botLevel bot)

  return bot

possibleDicesToRemovals :: [Dice] -> [(Int, Int)]
possibleDicesToRemovals diceList =
    [(index, value dice) | (index, dice) <- zip [1..] diceList, value dice == 1]

possibleDicesToRotations :: [Dice] -> [(Int, Int)]
possibleDicesToRotations diceList =
    [(index, value dice) | (index, dice) <- zip [1..] diceList, value dice /= 1]


getPlayerMove :: [Dice] -> IO (Int, Int, Int)
getPlayerMove diceList = do
    putStrLn "Escolha a jogada a ser feita:"
    putStrLn "1. Girar"

    if any (\dice -> value dice == 1) diceList
        then putStrLn "2. Retirar"
        else return ()

    putStrLn "Digite o número correspondente à ação desejada:"
    choicePlayer <- readLn
    case choicePlayer of
        1 -> do
            let dicesToRotations = possibleDicesToRotations diceList
            putStrLn "Possíveis rotações disponíveis:"
            mapM_ (\(i, option) -> putStrLn $ "Dado " ++ show i ++ ": " ++ show option) dicesToRotations
            let numOptions = length dicesToRotations
            putStrLn $ "Número de opções disponíveis para girar: " ++ show numOptions
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
        2 -> if any (\dice -> value dice == 1) diceList
                then do
                    let dicesToRemovals = possibleDicesToRemovals diceList
                    putStrLn "Possíveis remoções disponíveis:"
                    mapM_ (\(i, option) -> putStrLn $ "Dado " ++ show i ++ ": " ++ show option) dicesToRemovals
                    putStrLn "Escolha o dado para girar:"
                    index <- readLn
                    if any (\(i, option) -> i == index) dicesToRemovals
                        then return (2, index, 0)  -- Escolha 2 indica retirar
                        else do
                            putStrLn "Índice inválido. Tente novamente."
                            getPlayerMove diceList
                else do
                    putStrLn "Não há dados com face 1 para retirar."
                    getPlayerMove diceList
        _ -> do
            putStrLn "Opção inválida. Tente novamente."
            getPlayerMove diceList

