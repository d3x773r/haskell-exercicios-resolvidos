module MaximoDivisorComum where

mdc :: Int -> Int -> Int
mdc a b
 | b == 0 = a
 | b > 0 = mdc b (mod a b)
 | otherwise = mdc a (-b)