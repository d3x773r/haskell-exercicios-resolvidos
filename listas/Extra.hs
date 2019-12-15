-- Questão 1
removeEspaco :: String -> String
removeEspaco (x:xs)
 | x == ' ' = removeEspaco xs
 | otherwise = (x:xs)

-- Questão 2
getPalavra :: String -> String
getPalavra (x:xs)
 | xs == [] = [x]
 | (x == ',') || (x == ';') || (x == '.') || (x == '!') || (x == '?') || (x == ':') || (x == ' ') || (x == '-') = [] 
 | otherwise = x:getPalavra xs

-- Questão 3
subst :: String -> String -> String -> String
subst _ _ "" = []
subst s1 s2 s3
 | ((getPalavra s3) == s1) = s2++" "++removePalavraQ3 s3
 | otherwise = (getPalavra s3)++" "++(subst s1 s2 (removePalavraQ3 s3))

-- Questão 4
separa :: String -> Char -> [[String]]
separa "" _ = []
separa str c
 | head str == c = separa (removeEspaco str) c
 | otherwise = [getPalavra str]:separa (removePalavraQ4 str) c

-- Questão 5
junta :: [String] -> String -> String
junta str c
 | tail str == [] = head str
 | otherwise = (head str)++c++(junta (tail str) c)

-- Questão 6
agrupar :: [(String, Int)] -> [(String, [Int])]
agrupar [] = []
agrupar ((s, i):xs)
 | xs == [] = [(s, [i])]
 | otherwise = (s, membros ((s, i):xs) s):agrupar (repetidos xs s)

membros :: [(String,Int)] -> String -> [Int]
membros ((k,v):xs) key = [snd (k,v) | (k,v) <- ((k,v):xs), k == key]

repetidos :: [(String,Int)] -> String -> [(String,Int)]
repetidos ((k,v):xs) key = [(k,v) | (k,v) <- ((k,v):xs), k /= key]

-- Auxiliares
removePalavraQ3 :: String -> String
removePalavraQ3 [] = []
removePalavraQ3 (x:xs)
 | (head xs == '-') = xs
 | (x == ',') || (x == ';') || (x == '.') || (x == '!') || (x == '?') || (x == ':') || (x == ' ') = xs
 | otherwise = removePalavraQ3 xs

removePalavraQ4 :: String -> String
removePalavraQ4 [] = []
removePalavraQ4 (x:xs)
--  | (head xs == ['-']) = xs
 | (x == ',') || (x == ';') || (x == '.') || (x == '!') || (x == '?') || (x == ':') = xs
 | (x == ' ') = (x:xs)
 | otherwise = removePalavraQ4 xs