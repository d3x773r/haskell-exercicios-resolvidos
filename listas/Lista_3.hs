import Data.Char

-- Questão 1
menorMaior :: Int -> Int -> Int -> (Int, Int)
menorMaior x y z
-- Coluna X
 | (x <= y) && (y <= z) = (x, z)
 | (x <= y) && (z <= y) && (x <= z) = (x, y)
 -- Coluna Y
 | (y <= x) && (x <= z) = (y, z)
 | (y <= x) && (z <= x) && (y <= z) = (y, x)
 -- Coluna Z
 | (z <= x) && (x <= y) = (z, y)
 | (z <= x) && (y <= x) && (z <= y) = (z, x)
-- Questão 2
ordernaTripla :: (Int, Int, Int) -> (Int, Int, Int)
ordernaTripla (x, y, z)
-- Coluna X
 | (x <= y) && (y <= z) = (x, y, z)
 | (x <= z) && (z <= y) = (x, z, y)
 -- Coluna Y
 | (y <= x) && (x <= z) = (y, x, z)
 | (y <= z) && (z <= x) = (y, z, x)
 -- Coluna Z
 | (z <= x) && (x <= y) = (z, x, y)
 | (z <= y) && (y <= x) = (z, y, x)
 | otherwise = (x, x, x)
-- Questão 3
conversao :: Float -> ((Float, String), (Float, String), (Float, String))
conversao valor = ((valor, "Real"), (valor * 0.448, "Euro"), (valor * 0.547, "Dolar"))
-- Questão 4
type Ponto = (Float, Float)
type Reta = (Ponto, Ponto)
coordenadaX :: Ponto -> Float
coordenadaX ponto = fst ponto
coordenadaY :: Ponto -> Float
coordenadaY ponto = snd ponto
retaVertical :: Reta -> Bool
retaVertical reta = (fst (fst reta)) == (fst (snd reta))
-- Questão 5
-- digitosExtremos :: Integer -> (Integer, Integer)
-- digitosExtremos digito = (menorInteiro digito, maiorInteiro digito)
-- Questão 6
analisaLetra :: Char -> (Char, Char, Int)
analisaLetra c = (c, toUpper c, ord c)
-- Questão 7
-- equacao :: Int -> Int -> Int -> (Int, Int)
-- equacao a b c
--  | delta a b c > 0 = ( ((-b) + (sqrt(delta a b c)) `div` fromIntegral (2 * a)), ((-b) - (sqrt(delta a b c)) `div` fromIntegral(2 * a)))
--  | delta a b c == 0 = ( ((-b) + (sqrt(delta a b c))) `div` fromIntegral (2 * a), 0)
--  | otherwise = (-1, -1)
-- Questão 8
-- mmcMdc :: (Int, Int, Int) -> (Int, Int, Int)
-- mmcMdc (a, b, c) = 
-- Questão 9
type Pessoa = (String, Int, Float, Char)
getPessoa :: Int -> Pessoa
getPessoa x
 | x==1 = ("Cristina", 27, 1.66,'F')
 | x==2 = ("Flávio", 26, 1.85,'M')
 | x==3 = ("Mariana", 67, 1.55,'F')
 | x==4 = ("Cecília", 48, 1.78, 'M')
 | x==5 = ("Paulo", 24, 1.93, 'M')
 | x==6 = ("Clara", 38, 1.70,'F')
 | x==7 = ("Rodrigo", 12, 1.85, 'M')
 | x==8 = ("Giovana", 31, 1.58,'F')
 | x==9 = ("Daniel", 75, 1.74, 'M')
 | x==10 = ("Eduardo", 21, 1.69,'F')
 | otherwise = ("Acabou!",0, 0.0, 'x')
-- Questão 9. A)
numPessoa :: Int -> Int -> Int
numPessoa pessoaId1 pessoaId2
 | getIdade (getPessoa pessoaId1) > getIdade (getPessoa pessoaId2) = pessoaId1
 | otherwise = pessoaId2
-- Questão 9. B)
diferencaAltura :: Int -> Int -> Float
diferencaAltura a b = abs (getAltura (getPessoa a) - getAltura (getPessoa b))
-- Questão 9. C)
maiorAltura :: Int -> Int
maiorAltura x
 | x == 1 = 1
 | otherwise = maior x (maiorAltura (x-1))
-- Questão 10
base :: Int -> (Int, String, String, Char) 
base x 
 | x == 0 = (1793, "Pedro", "Mestre", 'M')
 | x == 1 = (1797, "Joana", "Mestre", 'F')
 | x == 2 = (1534, "Paulo", "Doutor", 'M')
 | x == 3 = (1267, "Carina", "Doutor", 'F')
 | x == 4 = (1790, "Helena", "Mestre", 'F')
 | x == 5 = (1689, "Jonas", "Doutor", 'M')
 | x == 6 = (1079, "Joao", "Mestre", 'M')
 | x == 7 = (1751, "Jose", "Doutor", 'M')
 | x == 8 = (1930, "Lucia", "Doutor", 'F')
 | x == 9 = (1389, "Rita", "Mestre", 'F')
 | x == 10 = (0, "", "", '0')
-- Questão 10. A)
qtdDoutores :: Int -> Int
qtdDoutores x
 | (getCodigo (base x)) == 0 = 0
 | (getDoutor (base x)) = succ ( qtdDoutores (x + 1) )
 | otherwise = qtdDoutores (x + 1)
-- Questão 10. B)
qtdMulheres :: Int -> Int
qtdMulheres x
 | getCodigo (base x) == 0 = 0
 | getSexoFeminino (base x) = succ (qtdMulheres (x + 1))
 | otherwise = qtdMulheres(x + 1)
-- Questão 10. C)
qtdMestreMasc :: Int -> Int
qtdMestreMasc x
 | getCodigo (base x) == 0 = 0
 | (getMestre (base x)) && (getSexoMasculino (base x)) = succ (qtdMestreMasc (x + 1))
 | otherwise = qtdMestreMasc(x + 1)
-- Questão 10. D)
qtdDoutoras :: Int -> Int
qtdDoutoras x
 | getCodigo (base x) == 0 = 0
 | (getDoutor (base x)) && (getSexoFeminino (base x)) = succ (qtdDoutoras (x + 1))
 | otherwise = qtdDoutoras(x + 1)
-- Questão 10. E)
profAntigo :: Int -> Int
profAntigo x
 | getCodigo (base x) == 0 = getMatricula (base 9)
 | otherwise = menor (getMatricula (base x)) (profAntigo (x + 1))
-- Questão 11
triangulo :: (Int, Int, Int) -> (String, Int)
triangulo (n1, n2, n3)
 | (n1 == n2) && (n2 == n3) = ("Equilatero", n1+n2+n3)
 | (n1 == n2) || (n1 == n3) || (n2 == n3) = ("Isosceles", n1+n2+n3)
 | otherwise = ("Escaleno", n1+n2+n3)



-- Auxiliares
-- menorInteiro :: Int -> Int
-- menorInteiro digito
--  | digito < mod digito 10 = 
--  | otherwise = menorInteiro (div digito 10)

delta :: Int -> Int -> Int -> Int
delta a b c = fromIntegral ((b ^ 2) - 4 * a * c)

getIdade :: Pessoa -> Int
getIdade (nome, idade, altura, sexo) = idade

getAltura :: Pessoa -> Float
getAltura (nome, idade, altura, sexo) = altura

maior :: Int -> Int -> Int
maior pessoaId1 pessoaId2
 | getAltura (getPessoa pessoaId1) >= getAltura (getPessoa pessoaId2) = pessoaId1
 | otherwise = pessoaId2

-- Tentando colocar numa List
-- getAll :: []

getCodigo :: (Int, String, String, Char) -> Int
getCodigo (cod, nome, formacao, sexo) = cod

getDoutor :: (Int, String, String, Char) -> Bool
getDoutor (cod, nome, formacao, sexo) = if formacao == "Doutor" then True else False

getSexoFeminino :: (Int, String, String, Char) -> Bool
getSexoFeminino (cod, nome, formacao, sexo)
 | sexo == 'F' = True
 | otherwise = False

getSexoMasculino :: (Int, String, String, Char) -> Bool
getSexoMasculino (cod, nome, formacao, sexo)
 | sexo == 'M' = True
 | otherwise = False

getMestre :: (Int, String, String, Char) -> Bool
getMestre (cod, nome, formacao, sexo) = if formacao == "Mestre" then True else False

getMatricula :: (Int, String, String, Char) -> Int
getMatricula (cod, nome, formacao, sexo) = cod

menor :: Int -> Int -> Int
menor a b
 | a < b = a
 | otherwise = b