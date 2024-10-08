module Source.Core.UI (getPlayerMove, getSetupData) where

import Source.Core.Board.Board (Board (..), getPossibleDicesToRemove, getPossibleDicesToRotate)
import Source.Core.Board.Dice (Dice (..))
import Source.Lib.Printer (printDice)
import Source.Lib.Reader (readBotLevel, readDiceByList, readInt, readMoveType, readNewDiceValue, readString)
import Source.Types.Move (Move (..), MoveType (..))
import Source.Types.SetupData (SetupData (..))

getSetupData :: IO SetupData
getSetupData = do
  numDices <- getNumberOfDices
  playerName <- getNameHumanPlayer
  botLevel <- readBotLevel
  return (SetupData {numDices = numDices, setupPlayerName = playerName, setupBotLevel = botLevel})
  where
    getNumberOfDices :: IO Int
    getNumberOfDices = do
      putStrLn "Quantos dados deseja jogar? "
      readInt (Just 1) Nothing

    getNameHumanPlayer :: IO String
    getNameHumanPlayer = do
      putStrLn "Qual o nome do jogador? "
      readString

getPlayerMove :: Board -> IO Move
getPlayerMove board = do
  moveType <- readMoveType board

  case moveType of
    Update -> do
      let dicesToRotate = getPossibleDicesToRotate board
      diceToRotateIndex <- readDiceByList dicesToRotate "Escolha um dado para girar:"
      let chosenDice = board !! diceToRotateIndex
      putStrLn "Dado escolhido: "
      printDice (diceToRotateIndex, chosenDice)
      newValue <- readNewDiceValue chosenDice
      return (UpdateMove {updateIndex = diceToRotateIndex, newValue = newValue})
    Remove ->
      do
        let dicesToRemovals = getPossibleDicesToRemove board
        diceToRemoveIndex <- readDiceByList dicesToRemovals "Escolha um dado para remover:"
        return (RemoveMove {removeIndex = diceToRemoveIndex})
