module ContarDigitosRepetidos where

contarDigitos :: Int -> Int -> Int
contarDigitos n k
 | n == 0 = 0
 | (mod n 10) == k = 1 + contarDigitos (div n 10) k
 | otherwise = contarDigitos (div n 10) k