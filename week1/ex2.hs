-- Using the Prelude functions length, take, drop, define two functins leftHalf e rightHalf that divide a list into two approximate halves.
x = [1, 2, 3, 4, 5, 6, 7]

y = [1, 2, 3, 4, 5, 6, 7]

halves :: [a] -> ([a], [a])
halves l = (take halfLen l, drop halfLen l)
  where
    halfLen = div (length l) 2
    -- halfLen = length l `div` 2

-- leftHalf = take (length list `div` 2) list
-- rightHalf = drop (length list `div` 2) list

-- length x = 7
-- take 2 x = [1,2] // Retorna o número de elementos definido, no caso, 2 elementos
-- drop 3 y = [4,5,6,7] // Exclui o número de elementos definido, no caso, os 3 primeiros, e retorna o restante
-- head x = 1
-- tail x [2,3,4,5,6,7]
-- init recebe todos os numeros, exceto o primeiro
-- reverse x = [7,6,5,4,3,2,1]
-- div: divisão inteira
-- mod: resto inteiro
-- :t - fala o tipo