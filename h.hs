playRound :: GameState -> IO ()
playRound gameState
  | nextPlayer gameState == Human = do
      (choice, index, value) <- getPlayerMove (board gameState)

      let actualizedState = case choice of
            1 ->
              let updatedboard = updateDiceByIndex (board gameState) (index - 1) value
               in gameState {board = updatedboard}
            2 ->
              let updatedboard = removeDiceByIndex (board gameState) (index - 1)
               in gameState {board = updatedboard}
      let chosenDice = board gameState !! (index - 1)
      printChosenMove choice (playerName (humanPlayer actualizedState)) chosenDice index value
      printStateCurrent (playerName (humanPlayer actualizedState)) (board actualizedState)
      if isGameOver actualizedState
        then putStrLn "Humano venceu"
        else playRound actualizedState
  | otherwise = do
      (choice, index, value) <- easyBotMove (board gameState)

      let actualizedState = case choice of
            1 ->
              let updatedboard = updateDiceByIndex (board gameState) index value
               in gameState {board = updatedboard}
            2 ->
              let updatedboard = removeDiceByIndex (board gameState) index
               in gameState {board = updatedboard}

      let chosenDice = board gameState !! index
      printChosenMove choice (playerName (botPlayer actualizedState)) chosenDice (index + 1) value
      printStateCurrent (playerName (botPlayer actualizedState)) (board actualizedState)

      if isGameOver actualizedState
        then putStrLn "Bot venceu"
        else playRound actualizedState