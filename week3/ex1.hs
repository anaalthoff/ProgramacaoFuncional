-- 3.1 Write your own recursive definitions for the following Prelude functions. Use different names in order to avoid clashes, e.g. define a function myand instead of and.

-- (a) and :: [Bool] -> Bool — test if all values are true;
myand :: [Bool] -> Bool
myand [] = True
-- Porque não há elementos falsos, então "todos os elementos são verdadeiros" é trivialmente True.
myand (x : xs) = x && myand xs

-- (b) or :: [Bool] -> Bool — test if some values are true;
myor :: [Bool] -> Bool
myor [] = False
-- Por convenção, se não há elementos, nenhum deles é True.
myor (x : xs) = x || myor xs

-- (c) concat :: [[a]] -> [a] — concat transforma uma lista de listas em uma única lista;
myconcat :: [[a]] -> [a]
myconcat [] = []
myconcat (x : xs) = x ++ myconcat xs

-- (d) replicate :: Int -> a -> [a] values; — produce a list with repetead
myreplicate :: Int -> a -> [a]
myreplicate n x
  | n <= 0 = []
  | otherwise = x : myreplicate (n - 1) x

-- (e) (!!) :: [a] -> Int -> a — index the n-th value in a list (starting from zero);
myindex :: [a] -> Int -> a
myindex [] _ = error "index out of bounds"
myindex (x : xs) 0 = x
myindex (x : xs) n = myindex xs (n - 1)

-- (f) elem :: Eq a => a -> [a] -> Bool — check if a value occurs in a list.
myelem :: Eq a => a -> [a] -> Bool
myelem _ [] = False
myelem y (x:xs) = (y == x) || myelem y xs