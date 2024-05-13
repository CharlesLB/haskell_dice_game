module Lib.Reader (initializingDices, initializingPlayers) where

import Core.Game (initializeDices, initializeHumanPlayer, initializeBotPlayer, game)
import Core.Players.Player (Player(..))
import Core.Players.BotPlayer (BotPlayer(..))
import Auxiliaries.ReaderLevelBot (getUserBotLevel) 

initializingDices :: IO ()
initializingDices = do 
  putStrLn "Quantos dados deseja jogar? "
  numDice <- readLn
  dices <- initializeDices numDice
  putStrLn $ "Configuração inicial dos dados: " ++ show dices

initializingPlayers :: IO ()
initializingPlayers = do
  putStrLn "Qual o nome do jogador? "
  nameHumanPlayer <- getLine
  human <- initializeHumanPlayer nameHumanPlayer 

  putStrLn "Qual o nome do bot? "
  nameBotPlayer <- getLine
  levelBotPlayer <- getUserBotLevel
  bot <- initializeBotPlayer nameBotPlayer levelBotPlayer

  putStrLn $ "O nome do jogador do tipo " ++ show (playerType human) ++ " é: " ++ playerName human
  putStrLn $ "O nome do jogador do tipo " ++ show (playerType bot) ++ " é: " ++ playerName bot ++ ". Ele é do nivel " ++ show (botLevel bot)