module FatorialQuad where
import Fatorial

fatorialQuad :: Int -> Int
fatorialQuad n
 | n == 0 = 1
 | otherwise = div (fatorial (2 * n)) (fatorial n) 