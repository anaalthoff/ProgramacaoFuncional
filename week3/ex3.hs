-- The function nub :: Eq a => [a]-> [a] from the Data.List module eliminates repeated ocorrences of values in a list. For example: nub "banana" = "ban". Write a recursive definition for this function; because the function is not in the Prelude, you don’t need to use a different name.

-- manter x, porque é a primeira ocorrência dele
-- remover todas as ocorrências de x em xs
-- aplicar nub ao restante
nub :: Eq a => [a]-> [a]
nub [] = []
nub (x:xs) = x : nub (filter (/= x) xs)
-- x → primeiro elemento da lista (vamos manter, porque é a primeira ocorrência)
-- filter :: (a -> Bool) -> [a] -> [a] -- recebe uma função booleana e uma lista, e retorna apenas os elementos que fazem a função retornar True.
-- Exemplo simples: filter even [1,2,3,4]  -- [2,4]
-- (\y -> y /= x) -- Essa é uma função anônima (lambda) que diz:
-- “Dado um elemento y, quero True se y não for igual a x”
-- Então:
-- y = 'b' → 'b' /= 'a' → True → mantém 'b'
-- y = 'a' → 'a' /= 'a' → False → mantém 'a'
-- y = 'n' → 'n' /= 'a' → True → mantém 'n'
-- y = 'a' → 'a' /= 'a' → False → remove 'a'
-- y = 'n' → 'n' /= 'a' → True → remove 'n'
-- y = 'a' → 'a' /= 'a' → False → remove 'a'
-- Resultado: "ban"