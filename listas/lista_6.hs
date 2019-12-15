import Data.List

somaQuad n = foldr (+) 0 (map (^2) [1..n])

produtorio lista = foldl (*) 1 lista 

somaQuadList lista = foldl (+) 0 (map (^2) lista)

somaDigitos :: Int -> Int
somaDigitos n   
    | div n 10 == 0 = n
    | otherwise = somaDigitos (div n 10) + somaDigitos (mod n 10)
    
ordena :: (Int,Int,Int,Int,Int) -> Int
ordena (a,b,c,d,e) = (sort (a:b:c:d:e:[])) !! 2

mediana :: [(Int,Int,Int,Int,Int)] -> [Int]
mediana lista = map ordena lista

primo :: Int -> Bool
primo 1 = False
primo 2 = True
primo n = (length [x | x <- [2..n-1], mod n x == 0]) == 0 

apenasPrimos :: [Int] -> [Int]
apenasPrimos lista = filter primo lista