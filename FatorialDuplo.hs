module FatorialDuplo where

fatorialDuplo :: Int -> Int
fatorialDuplo n
 | (n == 0) || (n == 1) = 1
 | otherwise = n * fatorialDuplo (n - 2)