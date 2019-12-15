-- Questão 1
-- A)
removerPrimeiro :: [Int] -> [Int]
removerPrimeiro (x:xs) = xs
-- B)
-- C)
removerUltimo :: [Int] -> [Int]
removerUltimo (x:xs) = init((x:xs))
-- D)
concatenarListas :: [Int] -> [Int] -> [Int]
concatenarListas (x:xs) (a:as) = (x:xs)++(a:as)
-- E)
maiorElem :: [Int] -> [Int]
maiorElem (x:xs) = [ x | x <- xs, x > (head xs) ]
-- F)
-- G)
-- qtdElem :: [Int] -> [Int]
-- qtdElem (x:xs) = [ x | x <- (x:xs) ]
-- H)
pertenceLista :: [Int] -> Int -> [Int]
pertenceLista (x:xs) num = [ x | x <- (x:xs), x == num ]
-- I)
nesimo :: [Int] -> Int -> Int
nesimo (x:xs) num = (x:xs) !! num
-- Questão 2
produto :: [Float] -> Float
produto (x:xs)
 | xs == [] = x
 | otherwise = x * (produto xs)
-- Questão 3
somaElem :: [(Int, Int, Int)] -> [Int]
somaElem ((n1, n2, n3):xs)
 | (n1,n2,n3) == (0,0,0) = []
 | otherwise = (n1+n2+n3):(somaElem xs)
-- Questão 4
-- Questão 5
-- Questão 6
-- apenasPrimos :: [Int] -> [Int]
-- apenasPrimos (x:xs)
--  | x == [] = []
--  | (x)
--  | otherwise = 