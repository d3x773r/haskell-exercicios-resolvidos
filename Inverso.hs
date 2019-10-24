module Inverso where

iguais :: Int -> Bool
iguais x = (x == inverso x)

expoente :: Int -> Int
expoente e
 | (e == 0) = 0
 | otherwise = 1 + expoente (div e 10)

inverso :: Int -> Int
inverso i
 | i == 0 = 0
 | otherwise = ((mod i 10) * (10 ^ expoente (div i 10))) + inverso (div i 10)