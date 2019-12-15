-- 1
somatorio :: Int -> Int
somatorio 0 = 0
somatorio n = n + somatorio(n - 1)
-- 2
multRec :: Int -> Int -> Int
multRec n1 n2
 | n2 == 0 = 0
 | otherwise = n1 + multRec n1 (n2 - 1)
-- 3
fatorial :: Int -> Int
fatorial n
 | n == 0 = 1
 | otherwise = n * fatorial (n - 1)
-- 4
fatorialDuplo :: Int -> Int
fatorialDuplo n
 | n == 0 = 1
 | otherwise = n * fatorialDuplo (n - 2)
-- 5
fatorialQuadruplo :: Int -> Int
fatorialQuadruplo n
 | n == 0 = 1
 | otherwise = div (fatorial (2 * n)) (fatorial n)
-- 6
superfatorial :: Int -> Int
superfatorial n
 | n == 0 = 1
 | otherwise = fatorial n * superfatorial (n - 1)
-- 7
produtoIntervalo :: Int -> Int -> Int
produtoIntervalo m n
 | m == n = m * n
 | otherwise = m * (produtoIntervalo (m + 1) (n - 1)) * n
-- 8
somaFunc :: Int -> Int -> Int
somaFunc n1 n2
 | n1 == 0 = n2
 | otherwise = succ (somaFunc (pred n1) n2)
-- 9 A)
resto :: Int -> Int -> Int
resto n1 n2
 | n1 < n2 = n1
 | otherwise = resto (n1 - n2) n2
-- 9 B)
quociente :: Int -> Int -> Int
quociente n1 n2
 | n1 < n2 = 0
 | otherwise = succ (quociente (n1-n2) n2)
-- 10
expoente :: Int -> Int -> Int
expoente n1 n2
 | n2 == 0 = 1
 | otherwise = n1 * expoente n1 (n2 - 1)
-- 11
raizQuadrada :: Int -> Int -> Int
raizQuadrada n c
 | (c ^ 2) <= n = raizQuadrada n (c+1)
 | otherwise = (c-1)
-- 12
mdc :: Int -> Int -> Int
mdc a b
 | b == 0 = a
 | b > 0 = mdc b (mod a b)
 | otherwise = mdc a (abs b)
-- 14
fibonacci :: Int -> Int
fibonacci n
 | n == 0 = 0
 | n == 1 = 0
 | n == 2 = 1
 | n > 2 = fibonacci (n - 1) + fibonacci (n - 2)
-- 15
teste :: Int -> Int -> Int
teste n1 n2
 | (mod n1 10) < 1 = 0
 | (mod n1 10) == n2 = 1
 | otherwise =  (teste (div n1 10) n2)