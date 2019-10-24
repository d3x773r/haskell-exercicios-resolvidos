module ProdutoIntervalo where

produtoIntervalo :: Int -> Int -> Int
produtoIntervalo m n
 | m == n = n
 | otherwise = m * produtoIntervalo (m + 1) n  