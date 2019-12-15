-- 1
maiorValor :: Int -> Int -> Int
maiorValor x y
 | x > y = x
 | otherwise = y
-- 2
doisValoresIguais :: Int -> Int -> Bool
doisValoresIguais x y = x == y
-- 3
tresValoresIguais :: Int -> Int -> Int -> Bool
tresValoresIguais x y z = doisValoresIguais x y && doisValoresIguais x z
-- 4
quatroValoresIguais :: Int -> Int -> Int -> Int -> Bool
quatroValoresIguais x y w z = tresValoresIguais x y w && doisValoresIguais x z
-- 5
parImpar :: Int -> Int
parImpar x
 | mod x 2 == 0 = x ^ 2
 | otherwise = x ^ 3
-- 6
potencia :: Int -> Int -> Int
potencia x y = x ^ y
-- 7
somaNumeros :: Int -> Int
somaNumeros x = div x 100 + div (mod x 100) 10 + mod x 10
-- 8
quantidadeIguais :: Int -> Int -> Int -> Int
quantidadeIguais x y z
 | tresValoresIguais x y z = 3
 | doisValoresIguais x y || doisValoresIguais x z || doisValoresIguais y z = 2
 | otherwise = 0
-- 9
maiorMedia :: Int -> Int -> Int -> Int
maiorMedia x y z
 | (x > media x y z) && (y > media x y z) = 2
 | (x > media x y z) && (z > media x y z) = 2
 | (y > media x y z) && (z > media x y z) = 2
 | x > media x y z = 1
 | y > media x y z = 1
 | z > media x y z = 1
 | otherwise = 0
 -- 10
quadrado :: Int -> Int
quadrado x = x ^ 2
-- 11
quadradoDuplo :: Int -> Int
quadradoDuplo x = quadrado x ^ 2
-- 12
calcCirc :: Float -> Float
calcCirc r = pi * (r ^ 2)
-- 13
ehPar :: Int -> String
ehPar x 
 | mod x 2 == 0 = "Par!"
 | otherwise = "Impar!"
-- 14
numerico :: Int -> Int
numerico x
 | x > 0 = 1
 | x < 0 = -1
 | otherwise = 0
-- 15
menor :: Int -> Int -> Int -> Int
menor x y z
 | (x < y) && (x < z) = x
 | (y < x) && (y < z) = y
 | (z < x) && (z < y) = z
 | (x == y) || (x == z) = x
 | otherwise = z
-- 16
andBoolean :: Bool -> Bool -> Bool -> Bool
andBoolean x y z = x && y && z
-- 17
orBoolean :: Bool -> Bool -> Bool -> Bool
orBoolean x y z = x || y || z
-- 18
xorBoolean :: Bool -> Bool -> Bool
xorBoolean x y
 | x == True && y == False = True
 | x == False && y == True = True
 | otherwise = False
-- 19
(#) :: String -> String -> String
(#) x y
 | x == y = x
 | otherwise = x ++ y
-- 20
calculadora :: Char -> Float -> Float -> Float
calculadora op x y
 | op == '*' = x * y
 | op == '/' = x / y
 | op == '-' = x - y
 | op == '+' = x + y
 | otherwise = -1
-- 21
precoPassagem :: Float -> Int -> Float
precoPassagem valor idade
 | idade > 60 = valor * 0.6
 | idade < 10 = valor * 0.5
 | idade < 2 = valor * 0.1
 | otherwise = valor

-- Auxiliares
media :: Int -> Int -> Int -> Int
media x y z = div (x + y + z) 3