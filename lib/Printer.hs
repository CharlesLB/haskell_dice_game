module Lib.Printer (printGameState) where

import Core.Game (GameMonad, GameState(..))

import Control.Monad.State

printGameState :: GameMonad ()
printGameState = do
  currentState <- get  -- Obtém o estado atual
  liftIO $ do  -- Utiliza liftIO para realizar operações de I/O dentro da monada
    putStrLn "Estado atual do jogo:"
    putStrLn $ "Jogador humano: " ++ show (humanPlayer currentState)
    putStrLn $ "Jogador bot: " ++ show (botPlayer currentState)
    putStrLn $ "Dados: " ++ show (dices currentState)