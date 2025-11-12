-- Show that you can write alternative definitions of the Prelude functions concat, replicate and (!!) using just list compreehensions (not recursion). Rename your functions to avoid clashes, e.g. myconcat, myreplicate, etc.

-- | concat: transforma [[a]] em [a]
myconcat :: [[a]] -> [a]
myconcat xss = [x | xs <- xss, x <- xs]
-- O que está antes do | (x) é o valor que será colocado na lista resultante.
-- Depois do | temos os geradores:
-- xs <- xss → para cada sublista xs dentro da lista de listas xss.
-- x <- xs → para cada elemento x dentro dessa sublista xs.

-- | replicate: cria uma lista com n cópias de um elemento
myreplicate :: Int -> a -> [a]
myreplicate n x = [x | _ <- [1..n]]
-- Int → quantas vezes você quer repetir o elemento.
-- a → o elemento que será repetido.
-- O que está antes do | (x) é o valor que será colocado na lista resultante.
-- O que está depois do | é o gerador:
-- [1..n] gera a lista [1,2,3,...,n].
-- _ significa que não usamos o valor do elemento, só precisamos de n iterações.
-- Para cada valor do gerador (1 a n), colocamos x na lista resultante.

-- | (!!): pega o i-ésimo elemento da lista (0-indexed)
myindex :: [a] -> Int -> a
myindex xs i = head [x | (x, idx) <- zip xs [0..], idx == i]
-- xs :: [a] → a lista de onde queremos pegar o elemento.
-- i :: Int → o índice (posição) do elemento que queremos (0-indexed, ou seja, o primeiro elemento é índice 0).
-- Retorna a → o elemento na posição i.
