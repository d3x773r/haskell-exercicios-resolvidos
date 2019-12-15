-- Questão 1 - Errada
somenteDigitos :: String -> String
somenteDigitos (x:xs) = [x | x <- (x:xs), digitos x]
-- Auxiliar
digitos :: Char -> Bool
digitos x
 | x == '0' = True
 | x == '1' = True
 | x == '2' = True
 | x == '3' = True
 | x == '4' = True
 | x == '5' = True
 | x == '6' = True
 | x == '7' = True
 | x == '8' = True
 | x == '9' = True
 | otherwise = False
-- Questão 2
removeChar :: Char -> String -> String
removeChar c (x:xs) = [x | x <- (x:xs), x /= c]
-- Questão 3
divide :: Int -> [Int] -> ([Int], [Int])
divide 0 (x:xs) = ([],(x:xs))
divide num (x:xs) = (metade1 num (x:xs), metade2 num (x:xs))
-- Auxiliar 1
metade1 :: Int -> [Int] -> [Int]
metade1 num (x:xs)
 | num == 0 = []
 | otherwise = x:metade1 (num-1) xs
-- Auxiliar 2
metade2 :: Int -> [Int] -> [Int]
metade2 num (x:xs)
 | num == 0 = (x:xs)
 | otherwise = metade2 (num-1) xs

-- Questão 4
intercala :: [Int] -> [Int] -> [Int]
intercala str1 str2 = str1++str2
-- Questão 5
-- uniao :: [Int] -> [Int] -> [Int]
-- uniao (x:xs) (y:ys) = [a | a <- (x:xs), contemLista ]
-- Questão 6
-- Questão 7
sequencia :: Int -> Int -> [Int]
sequencia qtd n
 | qtd == 0 = []
 | otherwise = (n+1):sequencia (qtd-1) (n+1)
-- Questão 8
contaOcorrencias :: Int -> [Int] -> Int -> Int
contaOcorrencias _ [] c = c
contaOcorrencias n (x:xs) c
 | n == x = contaOcorrencias n xs (c+1)
 | otherwise = contaOcorrencias n xs c
 --  Questão 9
unicaOcorrencia :: Int -> [Int] -> Int -> Bool
unicaOcorrencia _ [] _ = False
unicaOcorrencia n (x:xs) c
 | (n == x) && (c == 0) = unicaMesmo n xs
 | otherwise = unicaOcorrencia n xs c
-- Auxiliar
unicaMesmo :: Int -> [Int] -> Bool
unicaMesmo _ [] = True
unicaMesmo n (x:xs)
 | n == x = False
 | otherwise = unicaMesmo n xs
 
-- Outra forma
-- unicaOcorrencia :: Int -> [Int] -> Int -> Bool
-- unicaOcorrencia _ [] _ = False
-- unicaOcorrencia n (x:xs) c
--  | (n == x) && (c == 0) = unicaOcorrencia n xs (c+1)
--  | (n == x) && (c == 1) = False
--  | (xs == []) && (c == 1) = True
--  | otherwise = unicaOcorrencia n xs c

removerElementos :: [Int] -> [Int]
removerElementos [] = []
removerElementos (x:xs)
 | otherwise = x:removerElementos (removerProximos x xs)
-- Auxiliar
removerProximos :: Int -> [Int] -> [Int]
removerProximos _ [] = []
removerProximos n (x:xs) = [x | x <- (x:xs), x /= n]