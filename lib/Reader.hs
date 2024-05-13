module Lib.Reader (initializingGame) where

import Core.Game (newGameState, playGame)
import Core.Dice (Dice, initializeDices)
import Core.Players.Player (Player(..), PlayerType(..))
import Core.Players.HumanPlayer (HumanPlayer(..), initializeHumanPlayer) 
import Core.Players.BotPlayer (BotPlayer(..), BotPlayer(..), initializeBotPlayer)
import Auxiliaries.ReaderLevelBot (getUserBotLevel) 

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

initializingGame :: IO ()
initializingGame = do
  dices <- initializingDices
  human <- initializingHumanPlayer
  bot <- initializingBotPlayer

  let initialState = newGameState human bot dices Bot
  playGame initialState

  return ()  -- Conclui a ação IO ()