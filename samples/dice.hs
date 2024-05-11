-- Function to simulate rolling a dice and getting a random number between 1 and 6

-- Function to print the dice face corresponding to a number
printDice :: Int -> IO ()
printDice n = putStrLn $
  case n of
    1 -> " _____\n|     |\n|  ●  |\n|_____|"
    2 -> " _____\n|●    |\n|     |\n|____●|"
    3 -> " _____\n|●    |\n|  ●  |\n|____●|"
    4 -> " _____\n|●   ●|\n|     |\n|●___●|"
    5 -> " _____\n|●   ●|\n|  ●  |\n|●___●|"
    6 -> " _____\n|●   ●|\n|●   ●|\n|●___●|"
    _ -> "Invalid dice face"

main :: IO ()
main = do
  printDice 1
  printDice 2
  printDice 3
  printDice 4
  printDice 5
  printDice 6
