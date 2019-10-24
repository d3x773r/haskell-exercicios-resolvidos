media :: Int -> Int -> Int -> Int
media a b c = div (a + b + c) 3

maiorMedio :: Int -> Int -> Int -> Int
maiorMedio a b c
-- | (media a b c < a) && (media a b c < b) && (media a b c < c) = 3
 | ((media a b c < a) && (media a b c < b)) || ((media a b c < a) && (media a b c < c)) || ((media a b c < b) && (media a b c < c)) = 2
 | (media a b c < a) = 1
 | (media a b c < b) = 1
 | (media a b c < c) = 1
 | otherwise = 0