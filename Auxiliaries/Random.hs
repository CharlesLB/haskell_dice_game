module Auxiliaries.Random (randomInt, randomInts) where

-- Função para gerar um número aleatório entre 1 e 6
randomInt :: Int -> Int
randomInt seed = let nextSeed = (1103515245 * seed + 12345) `mod` 2147483648
                 in (nextSeed `mod` 6) + 1

-- Função para gerar uma lista infinita de números aleatórios entre 1 e 6 com base em uma semente inicial
randomInts :: Int -> [Int]
randomInts seed = let nextSeed = (1103515245 * seed + 12345) `mod` 2147483648
                      randomNumber = (nextSeed `mod` 6) + 1
                  in randomNumber : randomInts nextSeed