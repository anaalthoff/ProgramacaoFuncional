-- Write alternative definitions for the following Prelude functions. You should given them diferent names to avoid clashes, e.g. myappend instead of ++. 

-- (a) (++) :: [a]-> [a]-> [a], using foldr; 
myappend :: [a] -> [a] -> [a]
myappend xs ys = foldr (:) ys xs
-- xs ++ ys adiciona cada elemento de xs na frente de ys.

-- (b) concat :: [[a]]-> [a], using foldr;  concat transforma uma lista de listas em uma única lista;
myconcat :: [[a]]-> [a]
myconcat = foldr myappend []
-- foldr (++) [] xss → começa com [] e vai concatenando cada sublista

-- (c) reverse :: [a]-> [a], using foldr;
reverse :: [a]-> [a]
reverse xs = foldr (\x acc -> acc ++ [x]) [] xs
-- Para cada elemento x, adicionamos no final do acumulador acc
-- Esta versão tem complexidade O(n²) por causa do (++) em cada passo.

-- (d) reverse :: [a]-> [a], using foldl;
reverse' :: [a]-> [a]
reverse' xs = foldl (\acc x -> acc ++ [x]) [] xs
-- Para cada elemento x, adicionamos no início do acumulador acc
-- Esta versão tem complexidade O(n)

-- (e) elem :: Eq a => a-> [a]-> Bool, using any.
-- any verifica se algum elemento satisfaz uma condição
myelem :: Eq a => a -> [a] -> Bool
myelem y xs = any (== y) xs



-- A barra invertida \ → função anônima (lambda)
-- \x -> x + 1 -- Significa: "uma função que recebe x e retorna x + 1".
-- \ → “lambda”
-- x → parâmetro
-- -> → separa parâmetro do corpo da função
-- Exemplo:
-- ghci> (\x -> x + 1) 5
-- 6

-- foldr (:) ys xs
-- (:) é o operador de adicionar elemento na frente
-- foldr (:) ys xs percorre xs do último para o primeiro, colocando cada elemento na frente do acumulador (ys)
-- Visualizando:
-- xs = [1,2,3]
-- ys = [4,5]
-- foldr (:) ys xs
-- = 1 : (2 : (3 : [4,5]))
-- = [1,2,3,4,5]
-- Então foldr (:) ys xs é equivalente a xs ++ ys.

-- foldr (++) [] xss
-- [ [1,2], [3], [4,5] ] → queremos concatenar todas as sublistas em uma só
-- foldr (++) [] [[1,2],[3],[4,5]]
-- = [1,2] ++ ( [3] ++ ( [4,5] ++ [] ) )
-- = [1,2] ++ ( [3] ++ [4,5] )
-- = [1,2] ++ [3,4,5]
-- = [1,2,3,4,5]