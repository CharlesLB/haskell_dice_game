module Lib.Reader (initializingDices, initializingPlayers) where

import Core.Game (initializeGame, game)
import Core.Players.Player (Player(..), initializeHumanPlayer, initializeBotPlayer)

initializingDices :: IO ()
initializingDices = do 
  putStrLn "Quantos dados deseja jogar? "
  numDice <- readLn
  dices <- initializeGame numDice
  putStrLn $ "Configuração inicial dos dados: " ++ show dices

initializingPlayers :: IO ()
initializingPlayers = do
  putStrLn "Qual o nome do jogador? "
  nameHumanPlayer <- getLine
  human <- initializeHumanPlayer nameHumanPlayer 

  putStrLn "Qual o nome do bot? "
  nameBotPlayer <- getLine
  -- putStrLn "Qual o nível do bot "
  -- numDice <- readLn
  bot <- initializeBotPlayer nameBotPlayer 
  
  putStrLn $ "O nome do jogador do tipo " ++ show (playerType human) ++ " é: " ++ (playerName human) 