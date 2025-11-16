-- Write a definition of the function intersperse :: a-> [a]-> [a] from the Data.List module that intercalates a value between elements of a list. Examples: intersperse 0 [1,2,3] = [1,0,2,0,3] intersperse 0 [1] = [1] intersperse 0 [] = []
-- Hint: use recursion with pattern matching; you need only consider 3 distinct cases.
intersperse :: a-> [a]-> [a]
intersperse _ [] = []
intersperse _ [x] = [x]
intersperse n (x:xs) = x : n : intersperse n xs  -- caso 3: lista com 2+ elementos

-- Por que não pode fazer o seguinte?
-- intersperse [] = []  
-- intersperse [x] = [x]

-- Isso não funciona em Haskell porque:
-- [] e [x] estão tratando listas literais, não parâmetros genéricos.

-- A função original precisa ser genérica, ou seja, aceitar qualquer tipo a.
-- O correto é usar variáveis como parâmetros.

-- OBS: (x:xs) → pattern matching: x é o primeiro elemento, xs é o resto da lista
-- Os dois pontos (:) é o construtor de lista

