-- Sorting a list using the insertion sort algorithm. 

-- (a) Write a recursive definition of the function insert :: Ord a => a-> [a]-> [a] that inserts a value into an ordered list the maintaining the ascending order. Example: insert 2 [0,1,3,5] = [0,1,2,3,5].
-- Recebe um elemento x e uma lista já ordenada [a]
-- Retorna a lista com x inserido na posição correta, mantendo a ordenação crescente
insert :: Ord a => a-> [a]-> [a]
insert x [] = [x]
insert x (y:ys)
    | x <= y    = x : y : ys
    | otherwise = y : insert x ys

-- (b) Using insert, escreva a recursive definition of the function isort :: Ord a => [a]-> [a] that sorts the list using the insertion method: 
-- • the empty list [] is trivially sorted; 
-- • to sort a non-empty list, we first recursively sort the tail and then insert the head into the correct position.
isort :: Ord a => [a]-> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)