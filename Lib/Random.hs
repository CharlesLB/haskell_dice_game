module Lib.Random (randomInts) where

randomInts :: Int -> [Int]
randomInts seed =
  let nextSeed = (1103515245 * seed + 12345) `mod` 2147483648
      randomNumber = (nextSeed `mod` 6) + 1
   in randomNumber : randomInts nextSeed