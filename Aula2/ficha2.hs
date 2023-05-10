module Aula2 where
import Data.Char

--1)função recursiva que recebe uma lista de inteiros e adiciona um valor dado a cada elemento da lista
somaN :: [Int] -> Int -> [Int]
somaN [] n = []
somaN (h:t) n = (h+n) : somaN t n

--2)função que recebe uma lista de Strings e remove todas as Strings iniciadas por um dado caracter
removeS :: [String] -> Char -> [String]
removeS [] c = []
removeS (h:t) c | head h == c = removeS t c
                | otherwise = h : removeS t c

--2') remove char em string
removeC :: String -> Char -> String
removeC [] c = []
removeC (x:xs) c | x==c = removeC xs c
                 | otherwise = x : removeC xs c

--3)função recursiva que recebe uma lista de pares de inteiros e adiciona um valor dado à primeira componente de cada par
addP :: [(Int,Int)] -> Int -> [(Int,Int)]
addP [] _ = []
addP ((x,y):t) n = (x+n, y) : addP t n

--4)função recursiva que recebe uma lista, não vazia, de pares de inteiros e calcula qual o maior valor da segunda componente
maiorV :: [(Int,Int)] -> Int
maiorV [(x,y)] = y
maiorV ((x,y): l) | y > maiorV l = y 
                  | otherwise = maiorV l

--4')função recursiva que percorre uma lista de pares e soma os valores das segundas componentes
sumS :: [(Int,Int)] -> Int
sumS [] = 0
sumS ((x,y):l) = y + sumS l

--5)função que recebe um dígito e calcula o próximo dígito (considere que o sucessor de ’9’ é ’0’).
proxDigit :: Char -> Char
proxDigit '9' = '0'
proxDigit d = chr (ord d + 1)

--6)função que recebe uma lista de dígitos e substitui cada um deles pelo seu sucessor
subProx :: [Char] -> [Char]
subProx [] = []
subProx (h:t) = proxDigit h : subProx t

--7)função que recebe uma lista de vogais e substitui cada uma delas pela vogal seguinte (considere que a vogal seguinte a ’u’ é ’a’)
subVogal :: [Char] -> [Char]
subVogal [] = []
subVogal (h:t) = subVogalAux h : subVogal t

subVogalAux :: Char -> Char
subVogalAux 'u' = 'a'   
subVogalAux v | v == 'a' || v == 'e' = chr (ord v + 4)
              | otherwise = chr (ord v + 6)

          
--8)
type Nome = String
type Coordenada = (Int, Int) -- (0,0) canto inferior esquerdo
data Movimento = N | S | E | W deriving (Show,Eq) -- norte, sul, este, oeste
type Movimentos = [Movimento]
data PosicaoPessoa = Pos Nome Coordenada deriving (Show,Eq)

--a)função que calcule a posição de uma pessoa depois de executar uma sequência de movimentos
posicao :: PosicaoPessoa -> Movimentos -> PosicaoPessoa
posicao (Pos n (x,y)) [] = Pos n (x,y)
posicao (Pos n (x,y)) (h:t) | h == N = posicao (Pos n (x,y+1)) t
                            | h == S = posicao (Pos n (x,y-1)) t
                            | h == E = posicao (Pos n (x+1,y)) t
                            | otherwise = posicao (Pos n (x-1,y)) t

--b)Dada uma lista de posições de pessoas, atualize essa lista depois de todas executarem um movimento dado
posicoesM :: [PosicaoPessoa] -> Movimento -> [PosicaoPessoa]
posicoesM [] _ = []
posicoesM ((Pos n (x,y)):t) m | m == N = Pos n (x,y+1) : posicoesM t m
                              | m == S = Pos n (x,y-1) : posicoesM t m
                              | m == E = Pos n (x+1,y) : posicoesM t m
                              | otherwise = Pos n (x-1,y) : posicoesM t m

--c)Dada uma lista de posições de pessoas, atualize essa lista depois de todas as pessoas executarem uma mesma sequência de movimentos. Use as funções anteriormente definidas
posicoesMs:: [PosicaoPessoa] -> Movimentos -> [PosicaoPessoa]
posicoesMs [] _ = []
posicoesMs ((Pos n (x,y)):t) m = posicao (Pos n (x,y)) m : posicoesMs t m

--d)função que calcule quais as pessoas posicionadas mais a norte
pessoasNorte :: [PosicaoPessoa] -> [Nome]
pessoasNorte [] = []
pessoasNorte l = sepNome l (maiorY l)  
            where sepNome :: [PosicaoPessoa] -> Int -> [Nome]
                  sepNome [] _= []
                  sepNome ((Pos n (x,y)):t) m | y == m = n : sepNome t m
                                              | otherwise = sepNome t m 

maiorY :: [PosicaoPessoa] -> Int
maiorY [Pos n (x,y)] = y
maiorY ((Pos n (x,y)): t) | y > maiorY t = y 
                          | otherwise = maiorY t