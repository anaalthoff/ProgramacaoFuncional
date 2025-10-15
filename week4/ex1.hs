-- Definir tipos novos
-- Duas keywords: data e type
-- Fazer um tipo para representar um ponto 3D

-- Sinônimo:
-- type Point = (Double, Double, Double)
-- uclideanDist :: Point -> Point -> Double

-- Data
-- Diferença: com Data irá ter construtores. Uma função que instancia
-- Como definir o tipo árvore
-- Leaf e Node são construtores de valores, funções que recebem argumentos e devolvem instância do tipo tree. Leaf não tem argumentos, node tem argumentos, vai para a esquerda ou direita.
data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Eq, Show)

-- Maybe é uma estrutura de dados que tem ou não tem valor dentro.
-- data Maybe a = Nothing | Just a

-- “sumTree é uma função que, para qualquer tipo numérico a, recebe uma árvore de elementos a e devolve um número do mesmo tipo a.”
sumTree :: (Num a) => Tree a -> a
-- Caso base da recursão.
-- Se a árvore é apenas uma folha (Leaf), então não há valor numérico para somar.
-- Esse caso impede que a função continue recursivamente para sempre — é o “fim da árvore”.
sumTree Leaf = 0
-- Caso recursivo.
-- Quando a árvore tem um nó (Node) com: um valor val, uma subárvore esquerda left, e uma subárvore direita right.
-- Então: a soma total da árvore é o valor do nó + a soma da subárvore esquerda + a soma da subárvore direita.
sumTree (Node val left right) = val + sumTree left + sumTree right

myTree :: Tree Int
myTree = Node 40 (Node 38 Leaf (Node 39 Leaf Leaf)) (Node 3 Leaf Leaf)

data Lista a = Vazia | Carruagem a (Lista a)

-- "procurarNaLista é uma função que recebe uma lista de pares (k, v) e uma chave k, e devolve um Maybe v."
procurarNaLista :: (Eq k) => Lista (k, v) -> k -> Maybe v
-- Caso base: lista vazia
procurarNaLista Vazia _ = Nothing
-- Caso recursivo: lista não vazia
-- Fazre pattern Matching com os construtores
procurarNaLista (Carruagem (chave, valor) cauda) k
  -- É just valor, por ser um maybe
  | chave == k = Just valor
  | otherwise = procurarNaLista cauda k

-- Pattern matching - obter nomes para aprtes que representam a estrutra dos dados

-- Maybe
estaNaLista :: (Eq k) => Lista (k, v) -> k -> Bool
estaNaLista l k = case procurarNaLista l k of
  Nothing -> False
  Just _ -> True

--
data Arv a = Empty | No a (Arv a) (Arv a) deriving Show

-- Escreva uma definição recursiva da função abaixo
sumArv :: Num a => Arv a -> a
-- Caso base: árvore vazia
sumArv Empty = 0
-- Caso recursivo: árvore não vazia. Na definição da função, escrever 'No val esq dir' para extrair os valores do nó.
-- No é apenas o construtor, não o nó em si
sumArv (No val esq dir) = val + sumArv esq + sumArv dir