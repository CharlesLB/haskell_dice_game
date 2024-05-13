module Lib.Reader (initializingDices, initializingPlayers) where

import Core.Game (initializeGame, game)
import Core.Players.Player (Player(..), initializeHumanPlayer, initializeBotPlayer)
import Core.Players.BotPlayer (BotPlayer(..), BotLevel(..))

import Data.Char (toLower)

initializingDices :: IO ()
initializingDices = do 
  putStrLn "Quantos dados deseja jogar? "
  numDice <- readLn
  dices <- initializeGame numDice
  putStrLn $ "Configuração inicial dos dados: " ++ show dices

displayBotLevels :: IO ()
displayBotLevels = do
  putStrLn "Escolha o nível do bot:"
  putStrLn "1. Fácil"
  putStrLn "2. Médio"
  putStrLn "3. Difícil"

getUserBotLevel :: IO BotLevel
getUserBotLevel = do
  displayBotLevels
  putStrLn "Digite o número correspondente ao nível desejado:"
  choice <- getLine
  case map toLower choice of
    "1" -> return Easy
    "2" -> return Medium
    "3" -> return Hard
    _   -> do
      putStrLn "Opção inválida. Por favor, escolha uma opção válida (1, 2 ou 3)."
      getUserBotLevel

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