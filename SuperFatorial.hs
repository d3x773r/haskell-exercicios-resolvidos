module SuperFatorial where
import Fatorial

superFatorial :: Int -> Int
superFatorial n
 | n == 0 = 1
 | otherwise = (fatorial n) * (superFatorial (n - 1))