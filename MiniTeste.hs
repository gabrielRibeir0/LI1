{-|
Module : MiniTeste
Description : MiniTeste de LI1 que avalia os conceitos de Haskell, Haddock, HUnit e Git
Copyright : Gabriel Ribeiro <a104171@alunos.uminho.pt>;
Gabriel Ribeiro <a104171@alunos.uminho.pt>
-}
module MiniTeste where
{-|A função 'f' recebe uma lista de pares de inteiros e decrementa de 1 valor a segunta componente. 
Se a segunda componente ficar com o valor zero, o par deve ser removido da lista.

A função poderia ser definida da seguinte forma:

@
f [] = []
f ((x,y):t) | (y-1) == 0 = f t
            | otherwise = (x,y-1) : f t
@

== Exemplos de utilização:

>>> f [(2,1),(1,5),(4,6)]
[(1,4),(4,5)]

>>> f [(1,1),(2,0),(3,5)]
[(2,-1),(3,4)]

== Propriedades:
prop> f [(x,1)] = []
prop> y /= 1 => f [(x,y)] = [(x, y-1)]
-}
f :: [(Int,Int)] -> [(Int,Int)]
f [] = []
f ((x,y):t) | (y-1) == 0 = f t
            | otherwise = (x,y-1) : f t