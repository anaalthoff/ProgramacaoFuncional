-- We want to implement a safetail :: [a]-> [a] function that behaves like tail but gives the empty list when the argument is empty. Write three distinct definitions using conditional expressions, guards and patterns.

-- conditional expressions
safetail :: [a] -> [a]
safetail xs =
  if null xs
    then []
    else tail xs

-- guards
safetail' :: [a] -> [a]
safetail' xs
  | null xs = []
  | otherwise = tail xs

-- patterns
safetail'' :: [a] -> [a]
safetail'' [] = [] -- se a lista está vazia, retorna []
safetail'' (_ : xs) = xs -- se a lista tem cabeça e cauda, ignora a cabeça e retorna a cauda
