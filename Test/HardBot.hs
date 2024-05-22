import Source.Core.Players.BotPlayer (hardBotMove)
import Source.Types.Move (Move(..))
import Source.Core.Board.Dice (Dice(..))

expectative :: Move -> [(Int, Maybe Int)] -> Bool
expectative (UpdateMove index newVal) possibleMoves = (index, Just newVal) `elem` possibleMoves
expectative (RemoveMove index) possibleMoves = (index, Nothing) `elem` possibleMoves

testDiceBehavior :: [Int] -> [(Int, Maybe Int)] -> IO Bool
testDiceBehavior diceValues possibleMoves = do
    let dices = map Dice diceValues
    move <- hardBotMove dices
    return $ expectative move possibleMoves

main :: IO ()
main = do
    putStrLn "Um dado [N]:"
    let possibleMoves1 = [(0, Just 1)]
    let board1 = [2]
    result <- testDiceBehavior board1 possibleMoves1
    putStrLn $ "  Teste 1: " ++ show result

    let possibleMoves2 = [(0, Just 4), (0, Just 3), (0, Just 1)]
    let board2 = [5]
    result2 <- testDiceBehavior board2 possibleMoves2
    putStrLn $ "  Teste 2: " ++ show result2

    let possibleMoves3 = [(0, Just 2)]
    let board3 = [3]
    result3 <- testDiceBehavior board3 possibleMoves3
    putStrLn $ "  Teste 3: " ++ show result3

    putStrLn "\nDois dados [N, N]:"
    let possibleMoves1 = [(0, Just 1), (1, Just 4), (1, Just 3), (1, Just 1)]
    let board1 = [2, 5]
    result <- testDiceBehavior board1 possibleMoves1
    putStrLn $ "  Teste 1: " ++ show result

    let possibleMoves2 = [(0, Nothing), (1, Just 5), (1, Just 4), (1, Just 3), (1, Just 2)] 
    let board2 = [1, 6]
    result2 <- testDiceBehavior board2 possibleMoves2
    putStrLn $ "  Teste 2: " ++ show result2

    let possibleMoves3 = [(1, Just 1)]
    let board3 = [1, 2]
    result3 <- testDiceBehavior board3 possibleMoves3
    putStrLn $ "  Teste 3: " ++ show result3

    let possibleMoves4 = [(0, Nothing), (1, Nothing)]
    let board4 = [1, 1]
    result4 <- testDiceBehavior board4 possibleMoves4
    putStrLn $ "  Teste 4: " ++ show result4

    putStrLn "\nTrês dados [N, N, N]:"
    let possibleMoves1 = [(0, Just 1), (1, Just 4), (1, Just 3), (1, Just 1), (2, Just 1)]
    let board1 = [2, 5, 2]
    result <- testDiceBehavior board1 possibleMoves1
    putStrLn $ "  Teste 1: " ++ show result

    let possibleMoves2 = [(6, Just 2)] 
    let board2 = [2, 5, 2, 5, 2, 5, 3]
    result2 <- testDiceBehavior board2 possibleMoves2
    putStrLn $ "  Teste 2: " ++ show result2

    let possibleMoves3 = [(0, Just 2), (0, Just 3), (0, Just 4), (0, Just 5),
                          (1, Just 1), (1, Just 2), (1, Just 4), (1, Just 6),
                          (2, Just 1), (2, Just 3), (2, Just 5), (2, Just 6)]
    let board3 = [6, 3, 4]
    result3 <- testDiceBehavior board3 possibleMoves3
    putStrLn $ "  Teste 3: " ++ show result3