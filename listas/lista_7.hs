sucessor = \x -> x + 1 

par = \n -> if mod n 2 == 0 then True else False

e y = \x -> if x == False || y == False then False else True
    
fatorial = \x -> if x <=1 then 1 else x * fatorial(x - 1) 

fibonacci = \x -> if x < 2 then x else fibonacci(x - 1) + fibonacci(x - 2) 

pertence lista = \x -> if elem x (lista) then True else False 

concatenar y = \x -> y ++ x