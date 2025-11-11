-- Consider a function short :: [a]-> Bool that checks if a list has fewer than three elements.
-- (a) Write a definition of short using the length function.

-- conditional expressions
short :: [a]-> Bool
short xs = if length xs < 3 then True
    else False

short' :: [a]-> Bool
short' xs = length xs < 3

-- guards
short'' :: [a]-> Bool
short'' xs
    | length xs < 3 = True
    | otherwise = False

-- (b) Write another definition using multiple equations and patterns.
-- Pattern Matching (Multiple Equations)
short''' :: [a] -> Bool
short''' [] = True
short''' [_] = True
short''' [_,_] = True
short''' (_:_:_:_) = False

-- [] vs (:)
-- [] representa a lista vazia.
-- (:) é o construtor de listas, usado para separar a cabeça da cauda

-- Por que usamos (_ : _ : _) e não [_ : _ : _]?
-- [_ : _ : _] não é sintaxe válida em Haskell.
-- [x,y,z] é uma lista literal de exatamente 3 elementos.
-- Quando usamos (:), estamos pattern matching de forma recursiva:
-- (_ : _ : _) → lista com pelo menos 2 elementos
-- (_ : _ : _ : _) → lista com pelo menos 3 elementos