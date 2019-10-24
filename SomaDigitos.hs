somaDigitos :: Int -> Int -- 123
somaDigitos numero = (div numero 100) + (div (mod numero 100) 10) + (mod numero 10)