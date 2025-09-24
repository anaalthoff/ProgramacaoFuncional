-- drop 3 y = [4,5,6,7]
-- Busca o último elemento da lista
last' :: [a] -> a
last' l = head (drop (length l - 1) l)

last'' :: [a] -> a
last'' l = head (reverse l)

-- Recebe todos, exceto o último elemento
init' :: [a] -> [a]
-- take 2 x = [1,2] // Retorna o número de elementos definido, no caso, 2 elementos
init' l = take (length l - 1) l

-- Recebe todos, exceto o último elemento
init'' :: [a] -> [a]
init'' l = reverse (tail (reverse l))
