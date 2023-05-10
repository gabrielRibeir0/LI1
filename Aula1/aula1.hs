--2) a)função que calcule a área de um quadrado, dado o seu lado
areaQ :: Float -> Float
areaQ l = l * l 

--b)função que calcule o perímetro de um rectângulo, dados o comprimento e a largura
perimetroR :: Float -> Float -> Float
perimetroR c l = 2*c + 2*l 

--c)função que verifique se um caractere pertence a uma string
pertenceChar :: Char -> String -> Bool
pertenceChar c s = elem c s

--d) função que recebe uma lista não vazia e que remove o primeiro elemento da lista, se ela tiver um número par de elementos. Se tiver um número impar de elementos, remove o último elemento da lista
rmElem :: [a] -> [a]
rmElem l = if mod (length l) 2 == 0 then tail l else init l

rmElemb :: [a] -> [a]
rmElemb l = if even (length l)then tail l else init l

--e)função que recebe uma lista não vazia e devolve um par com o primeiro e último elemento da lista
calculaPar :: [a] -> (a,a)
calculaPar l = (head l, last l)

--f)função que recebe uma lista de strings, representando os nomes de uma pessoa, e produz um par com o primeiro e o último nome dessa pessoa
abreviaNome :: [String] -> (String, String)
abreviaNome n = calculaPar n

--g)função que recebe um par de listas (xs,ys) e devolve um par em que a primeira componente é o elemento que está à cabeça da primeira lista xs, e a segunda componente é a lista ys inalterada
listas :: ([a],[b]) -> (a,[b])
listas (xs,ys) = (head xs,ys)

--h)função que recebe uma lista de nomes de uma pessoa (uma lista de valores tipo string) e produz uma string com a inicial do primeiro nome, seguida do caracter ’.’ seguida do último nome.
nomes :: [String] -> String
nomes l = head (head l) : "." ++ last l -- ':' para juntar elementos ao início de uma lista (neste caso string que é lista de Char) // '++' para juntar strings