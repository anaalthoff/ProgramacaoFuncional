-- Consider the definition of function f below: 
-- f :: [a]-> (a,[a]) 
-- f (_:_:x:y) = (x,y) 
-- a) Explain concisely what function f computes. 
-- b) Implement function f using functions from the Prelude, and without using patter nmatching nor recursion.

f :: [a]-> (a,[a]) 
-- f é uma função que recebe uma lista de elementos do tipo a e devolve um par, em que o primeiro elemento é do tipo a e o segundo é uma lista de a.
f (_:_:x:y) = (x,y) 
-- f ignora os dois primeiros elementos, seguidos de um elemento x e do restante da lista y, devolve o par (x, y).

x = [1, 2, 3, 4, 5, 6, 7]
-- implemente length, tail, head, init, reverse, t

f' x = (head (drop 2 x), drop 3 x)

f'' l = (l !! 2, drop 3 l)
-- !! é o operador de acesso por índice em Haskell
-- Ele pega o elemento na posição indicada, contando a partir de 0.
-- Depois descarta os 3 primeiros elementos com o drop 3

-- Para executar no terminal:
-- :l week5/listFunctionAnalysis.hs
-- Depois:
-- f' x
-- f'' x

-- length (snd (f' x))   -- 4
-- head (snd (f' x))     -- 4
-- tail (snd (f' x))     -- [5,6,7]
-- init (snd (f' x))     -- [4,5,6]
-- reverse (snd (f' x))  -- [7,6,5,4]