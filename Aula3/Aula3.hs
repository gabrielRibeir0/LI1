--2)recebe uma lista e desloca cada elemento da lista, n posições para a direita. Os elementos do final da lista são colocados no início
direita :: [a] -> Int -> [a]
direita l 0 = l
direita l n = direita (last l : init l) (n -1)
--3)função semelhante à anterior, mas que desloca os elementos para a esquerda
esquerda :: [a] -> Int -> [a]
esquerda l 0 = l
esquerda l n = esquerda (tail l ++ [head l] ) (n - 1)

--4)função que dada uma matriz não vazia, troca a primeira linha com a última    matriz?
swapRows :: [[a]] -> [[a]]
swapRows (h:t) = last t : init t ++ [h]

--5)função que recebe uma matriz não vazia e troca a primeira coluna com a última
--swapColumns :: [[a]] -> [[a]]

--6)função recursiva que procure a posição de um elemento numa lista (posição da primeira ocorrência). Devolve (-1) caso o elemento não ocorra na lista
pos :: Eq a => a -> [a] -> Int
pos _ [] = -1
pos a (h:t) | a == h = 0
            | otherwise = 1 + pos a t

--7)função recursiva que substitua um elemento de uma posição numa lista, por outro valor dado
subsElem :: [a] -> Int -> a -> [a]
subsElem  (_:t) 0 a = a : t
subsElem  (h:t) n a = h : subsElem t (n-1) a

--8)função recursiva que procure a posição de um elemento numa matriz (posição da primeira ocorrência que encontrar). Use funções anteriormente definidas

--9)função recursiva que substitua um elemento de uma posição dada numa matriz, por outro valor dado. Use funções anteriormente definidas

--10)Use as funções pré-definidas take, drop e/ou splitAt para reformular algumas das funções anteriores